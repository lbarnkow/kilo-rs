/*
kilo-rs a Rust port of the ["Build Your Own Text Editor" tutorial](https://viewsourcecode.org/snaptoken/kilo/index.html)
*/

use std::{error::Error, io::Read, io::Write, ops::RangeInclusive, os::fd::AsRawFd, time::Instant};
use thiserror::Error;

#[derive(Debug, Error)]
enum KiloError {
    #[error("io error")]
    IoError(#[from] std::io::Error),
    #[error("ioctl syscall not yet implemented!")]
    IoctlNotYetImplemented,
    #[error("get_window_size fallback function failed!")]
    GetWindowSizeFallbackError,
    #[error("can't convert key code '{0}' into char!")]
    ParseKeyError(u8),
}

/*** defines ***/

const KILO_QUIT_TIMES: usize = 3;

const KILO_TAB_STOP: usize = 4;

const CTRL_BIT_MASK: u8 = 0x1f;

fn ctrl_key(k: char) -> u8 {
    k as u8 & CTRL_BIT_MASK
}

/*** data ***/

#[derive(Clone, Copy, PartialEq)]
enum EditorHighlight {
    Normal,
    Comment,
    MlComment,
    Keyword1,
    Keyword2,
    String,
    Number,
    Match,
}

impl Default for EditorHighlight {
    fn default() -> Self {
        Self::Normal
    }
}

#[derive(Clone, Copy, PartialEq)]
enum EditorHighlightFlags {
    Numbers,
    Strings,
}

struct EditorSyntax {
    file_type: &'static str,
    file_match: &'static [&'static str],
    keywords: &'static [&'static str],
    singleline_comment_start: &'static str,
    multiline_comment_start: &'static str,
    multiline_comment_end: &'static str,
    flags: &'static [EditorHighlightFlags],
}

static EDITOR_SYNTAX: &[EditorSyntax] = &[
    EditorSyntax {
        file_type: "c",
        file_match: &[".c", ".h", ".cpp"],
        keywords: &[
            "switch",
            "if",
            "while",
            "for",
            "break",
            "continue",
            "return",
            "else",
            "struct",
            "union",
            "typedef",
            "static",
            "enum",
            "class",
            "case",
            "int|",
            "long|",
            "double|",
            "float|",
            "char|",
            "unsigned|",
            "signed|",
        ],
        singleline_comment_start: "//",
        multiline_comment_start: "/*",
        multiline_comment_end: "*/",
        flags: &[EditorHighlightFlags::Numbers, EditorHighlightFlags::Strings],
    },
    EditorSyntax {
        file_type: "rs",
        file_match: &[".rs"],
        keywords: &[
            "as", "break", "const", "continue", "crate", "else", "enum", "extern", "false", "fn",
            "for", "if", "impl", "in", "let", "loop", "match", "mod", "move", "mut", "pub", "ref",
            "return", "self", "Self", "static", "struct", "super", "trait", "true", "type",
            "unsafe", "use", "where", "while", "async", "await", "dyn", "bool|", "String|", "str|",
            "char|", "u8|", "u16|", "u32|", "u64|", "u128|", "i8|", "i16|", "i32|", "i64|",
            "i128|", "usize|", "isize|", "f32|", "f64|",
        ],
        singleline_comment_start: "//",
        multiline_comment_start: "/*",
        multiline_comment_end: "*/",
        flags: &[EditorHighlightFlags::Numbers, EditorHighlightFlags::Strings],
    },
];

#[derive(Default)]
struct EditorRow {
    idx: usize,
    chars: String,
    render: String,
    hl: Vec<EditorHighlight>,
    hl_open_comment: bool,
}

enum FindDirection {
    Forward,
    Backward,
}

struct EditorFind {
    last_match: Option<usize>,
    direction: FindDirection,
    saved_hl: Option<Vec<EditorHighlight>>,
    saved_hl_line: usize,
}

impl Default for EditorFind {
    fn default() -> Self {
        Self {
            last_match: None,
            direction: FindDirection::Forward,
            saved_hl: None,
            saved_hl_line: 0,
        }
    }
}

struct EditorConfig {
    cursor_x: usize,
    cursor_y: usize,
    render_x: usize,
    row_offset: usize,
    col_offset: usize,
    screen_rows: usize,
    screen_cols: usize,
    rows: Vec<EditorRow>,
    dirty: bool,
    filename: Option<String>,
    status_msg: String,
    status_msg_time: Instant,
    syntax: Option<&'static EditorSyntax>,
    orig_termios: termios::Termios,
    quit_times: usize,
    find: EditorFind,
}

/*** terminal ***/

fn die<E>(editor_config: Option<&mut EditorConfig>, e: E) -> !
where
    E: Error,
{
    if let Some(editor_config) = editor_config {
        disable_raw_mode(editor_config);
        editor_refresh_screen(editor_config).unwrap_or(());
    }

    print!("ERROR: {e}\r\n");
    std::process::exit(1);
}

fn disable_raw_mode(editor_config: &EditorConfig) {
    let fd = std::io::stdin().as_raw_fd();
    // discard errors
    termios::tcsetattr(fd, termios::TCSAFLUSH, &editor_config.orig_termios).unwrap_or(());
}

fn enable_raw_mode() -> termios::Termios {
    let fd = std::io::stdin().as_raw_fd();

    let mut raw = termios::Termios::from_fd(fd).unwrap_or_else(|e| die(None, e));
    let orig_termios = raw.clone();

    raw.c_iflag &=
        !(termios::BRKINT | termios::ICRNL | termios::INPCK | termios::ISTRIP | termios::IXON);
    raw.c_oflag &= !termios::OPOST;
    raw.c_cflag |= termios::CS8;
    raw.c_lflag &= !(termios::ECHO | termios::ICANON | termios::IEXTEN | termios::ISIG);
    raw.c_cc[termios::VMIN] = 0;
    raw.c_cc[termios::VTIME] = 1;

    termios::tcsetattr(fd, termios::TCSAFLUSH, &raw).unwrap_or_else(|e| die(None, e));
    orig_termios
}

fn read_key_helper() -> Result<Option<u8>, KiloError> {
    let mut stdin = std::io::stdin();
    let mut buf = [0_u8; 1];

    match stdin.read(&mut buf) {
        Err(e) => match e.kind() {
            std::io::ErrorKind::Interrupted => return Ok(None),
            _ => return Err(e.into()),
        },
        Ok(1) => Ok(Some(buf[0])),
        Ok(_) => Ok(None),
    }
}

fn editor_read_key() -> Result<KeyEvent, KiloError> {
    static DIGITS: RangeInclusive<u8> = ('0' as u8)..=('9' as u8);

    let mut buf = [0_u8; 3];

    loop {
        buf[0] = match read_key_helper()? {
            None => continue,
            Some(c) => c,
        };

        let event = match buf[0] {
            0x1b => {
                for i in 0..=1 {
                    buf[i] = match read_key_helper()? {
                        None => return Ok(KeyEvent::Escape),
                        Some(c) => c,
                    };
                }
                if buf[0] == '[' as u8 {
                    if DIGITS.contains(&buf[1]) {
                        buf[2] = match read_key_helper()? {
                            None => return Ok(KeyEvent::Escape),
                            Some(c) => c,
                        };
                        match (buf[1], buf[2]) {
                            (a, b) if a == '1' as u8 && b == '~' as u8 => KeyEvent::HomeKey,
                            (a, b) if a == '3' as u8 && b == '~' as u8 => KeyEvent::DeleteKey,
                            (a, b) if a == '4' as u8 && b == '~' as u8 => KeyEvent::EndKey,
                            (a, b) if a == '5' as u8 && b == '~' as u8 => KeyEvent::PageUp,
                            (a, b) if a == '6' as u8 && b == '~' as u8 => KeyEvent::PageDown,
                            (a, b) if a == '7' as u8 && b == '~' as u8 => KeyEvent::HomeKey,
                            (a, b) if a == '8' as u8 && b == '~' as u8 => KeyEvent::EndKey,
                            _ => KeyEvent::Escape,
                        }
                    } else {
                        match buf[1] {
                            c if c == 'A' as u8 => KeyEvent::MoveCursor(Direction::Up),
                            c if c == 'B' as u8 => KeyEvent::MoveCursor(Direction::Down),
                            c if c == 'C' as u8 => KeyEvent::MoveCursor(Direction::Right),
                            c if c == 'D' as u8 => KeyEvent::MoveCursor(Direction::Left),
                            c if c == 'H' as u8 => KeyEvent::HomeKey,
                            c if c == 'F' as u8 => KeyEvent::EndKey,
                            _ => KeyEvent::Escape,
                        }
                    }
                } else if buf[0] == 'O' as u8 {
                    match buf[1] {
                        c if c == 'H' as u8 => KeyEvent::HomeKey,
                        c if c == 'H' as u8 => KeyEvent::EndKey,
                        _ => KeyEvent::Escape,
                    }
                } else {
                    KeyEvent::Escape
                }
            }
            i if i & CTRL_BIT_MASK == i => KeyEvent::Ctrl(i),
            i if i == 0b_0111_1111 => KeyEvent::Backspace,
            c => KeyEvent::Key(char::from_u32(c as u32).ok_or(KiloError::ParseKeyError(c))?),
        };

        return Ok(event);
    }
}

fn get_cursor_position() -> Result<(usize, usize), KiloError> {
    let mut stdout = std::io::stdout();

    stdout.write("\x1b[6n".as_bytes())?;
    stdout.flush()?;

    let mut stdin = std::io::stdin();
    let mut buf = [0_u8; 32];
    let mut i = 0;

    while i < buf.len() {
        match stdin.read(&mut buf[i..=i]) {
            Err(_) => break,
            Ok(n) if n != 1 => break,
            _ => (),
        }
        if buf[i] == 'R' as u8 {
            break;
        }
        i += 1;
    }

    let buf: String = buf
        .into_iter()
        .filter(|c| *c != 0)
        .map(|c| c as u32)
        .map(|c| char::from_u32(c).unwrap_or('\0'))
        .collect();

    if !buf.starts_with("\x1b[") || !buf.ends_with("R") {
        return Err(KiloError::GetWindowSizeFallbackError);
    }

    let mut buf = (&buf[2..buf.len() - 1]).split(";");
    let screen_rows = buf.next().unwrap().parse().unwrap();
    let screen_cols = buf.next().unwrap().parse().unwrap();

    Ok((screen_rows, screen_cols))
}

fn ioctl_window_size() -> Result<(usize, usize), KiloError> {
    Err(KiloError::IoctlNotYetImplemented)
}

fn get_window_size(_editor_config: &EditorConfig) -> Result<(usize, usize), KiloError> {
    let size = match ioctl_window_size() {
        Err(_) => {
            let mut stdout = std::io::stdout();
            stdout.write("\x1b[999C\x1b[999B".as_bytes())?;
            get_cursor_position()?
        }
        Ok(size) => size,
    };

    Ok(size)
}

/*** syntax highlighting ***/

fn is_seperator(c: char) -> bool {
    c.is_ascii_whitespace() || c == '\0' || ",.()+-/*=~%<>[]{};".contains(c)
}

fn editor_update_syntax(editor_config: &mut EditorConfig, row: usize) {
    let mut in_comment = row > 0 && editor_config.rows[row - 1].hl_open_comment;

    let row = &mut editor_config.rows[row];
    let hl = &mut row.hl;

    hl.resize(row.render.len(), Default::default());
    hl.fill(Default::default());

    let Some(syntax) = &editor_config.syntax else {
        return;
    };

    let keywords = syntax.keywords;

    let scs = syntax.singleline_comment_start;
    let mcs = syntax.multiline_comment_start;
    let mce = syntax.multiline_comment_end;

    let mut prev_sep = true;
    let mut prev_hl = Default::default();
    let mut in_string = '\0';

    let mut i = 0;
    let rsize = row.render.chars().count();
    while i < rsize {
        let c = row.render.chars().skip(i).take(1).next().unwrap();

        if !scs.is_empty() && in_string == '\0' && !in_comment {
            if row.render[i..].starts_with(scs) {
                hl[i..].fill(EditorHighlight::Comment);
                break;
            }
        }

        if !mcs.is_empty() && !mce.is_empty() && in_string == '\0' {
            if in_comment {
                hl[i] = EditorHighlight::MlComment;
                if row.render[i..].starts_with(mce) {
                    hl[i..i + mce.len()].fill(EditorHighlight::MlComment);
                    i += mce.len();
                    in_comment = false;
                    prev_sep = true;
                    continue;
                } else {
                    i += 1;
                    continue;
                }
            } else if row.render[i..].starts_with(mcs) {
                hl[i..i + mcs.len()].fill(EditorHighlight::MlComment);
                i += mcs.len();
                in_comment = true;
                continue;
            }
        }

        if syntax.flags.contains(&EditorHighlightFlags::Strings) {
            if in_string != '\0' {
                hl[i] = EditorHighlight::String;
                if c == '\\' && i + 1 < row.render.len() {
                    hl[i + 1] = EditorHighlight::String;
                    i += 2;
                    continue;
                }
                if c == in_string {
                    in_string = '\0';
                }
                i += 1;
                prev_sep = true;
                continue;
            } else {
                if c == '"' || c == '\'' {
                    in_string = c;
                    hl[i] = EditorHighlight::String;
                    i += 1;
                    continue;
                }
            }
        }

        if syntax.flags.contains(&EditorHighlightFlags::Numbers) {
            if (c.is_ascii_digit() && (prev_sep || prev_hl == EditorHighlight::Number))
                || (c == '.' && prev_hl == EditorHighlight::Number)
            {
                hl[i] = EditorHighlight::Number;
                prev_hl = hl[i];
                prev_sep = false;
                i += 1;
                continue;
            }
        }

        if prev_sep {
            let mut matched_keyword = false;
            for keyword in keywords {
                let kw2 = if keyword.ends_with('|') {
                    EditorHighlight::Keyword2
                } else {
                    EditorHighlight::Keyword1
                };
                let keyword = if kw2 == EditorHighlight::Keyword1 {
                    *keyword
                } else {
                    &keyword[0..keyword.len() - 1]
                };

                if row.render[i..].starts_with(keyword) {
                    let next = row.render[i + keyword.len()..].chars().next();
                    if next.is_none() || is_seperator(next.unwrap()) {
                        hl[i..i + keyword.len()].fill(kw2);
                        i += keyword.len();
                        matched_keyword = true;
                        break;
                    }
                }
            }
            if matched_keyword {
                prev_sep = false;
                continue;
            }
        }

        prev_hl = hl[i];
        prev_sep = is_seperator(c);
        i += 1;
    }

    let at = row.idx;
    let changed = row.hl_open_comment != in_comment;
    row.hl_open_comment = in_comment;
    if changed && at + 1 < editor_config.rows.len() {
        editor_update_syntax(editor_config, at + 1);
    }
}

fn editor_syntax_to_color(hl: EditorHighlight) -> u32 {
    match hl {
        EditorHighlight::Comment | EditorHighlight::MlComment => 36,
        EditorHighlight::Keyword1 => 33,
        EditorHighlight::Keyword2 => 32,
        EditorHighlight::String => 35,
        EditorHighlight::Number => 31,
        EditorHighlight::Match => 34,
        _ => 37,
    }
}

fn editor_select_syntax_highlight(editor_config: &mut EditorConfig) {
    editor_config.syntax = None;
    let Some(filename) = &editor_config.filename else {
        return;
    };

    for editor_syntax in EDITOR_SYNTAX {
        for file_match in editor_syntax.file_match {
            let is_ext = file_match.starts_with('.');
            if (is_ext && filename.ends_with(file_match))
                || (!is_ext && filename.contains(file_match))
            {
                editor_config.syntax = Some(editor_syntax);

                for row in 0..editor_config.rows.len() {
                    editor_update_syntax(editor_config, row);
                }

                return;
            }
        }
    }
}

/*** row operations ***/

fn editor_row_cursor_x_to_render_x(row: &EditorRow, cursor_x: usize) -> usize {
    (&row.chars[0..cursor_x])
        .chars()
        .fold(0, |mut render_x, c| {
            if c == '\t' {
                render_x += (KILO_TAB_STOP - 1) - (render_x % KILO_TAB_STOP);
            }
            render_x + 1
        })
}

fn editor_row_render_x_to_cursor_x(row: &EditorRow, render_x: usize) -> usize {
    let mut cur_rx = 0;
    for (cx, c) in row.chars.char_indices() {
        if c == '\t' {
            cur_rx += (KILO_TAB_STOP - 1) - (cur_rx % KILO_TAB_STOP);
        }
        cur_rx += 1;

        if cur_rx > render_x {
            return cx;
        }
    }
    return row.chars.len();
}

fn editor_update_row(editor_config: &mut EditorConfig, row: usize) {
    let row_ref = &mut editor_config.rows[row];
    let mut render = String::with_capacity(row_ref.chars.len());

    row_ref.chars.chars().for_each(|c| {
        if c == '\t' {
            (0..KILO_TAB_STOP).for_each(|_| render.push(' '));
        } else {
            render.push(c);
        }
    });
    row_ref.render = render;
    // editor_config.dirty = true;

    editor_update_syntax(editor_config, row);
}

fn editor_insert_row(editor_config: &mut EditorConfig, at: usize, s: &str) {
    if at > editor_config.rows.len() {
        return;
    }

    let row = EditorRow {
        idx: at,
        chars: s.to_owned(),
        render: String::with_capacity(0),
        hl: vec![],
        hl_open_comment: false,
    };
    editor_config.rows.insert(at, row);

    for row in &mut editor_config.rows[(at + 1)..] {
        row.idx += 1;
    }

    editor_update_row(editor_config, at);
    editor_config.dirty = true;
}

fn editor_del_row(editor_config: &mut EditorConfig, at: usize) -> Option<EditorRow> {
    if at >= editor_config.rows.len() {
        return None;
    }
    let row = editor_config.rows.remove(at);

    for row in &mut editor_config.rows[at..] {
        row.idx -= 1;
    }

    editor_config.dirty = true;
    Some(row)
}

fn editor_row_insert_char(editor_config: &mut EditorConfig, row: usize, mut at: usize, c: char) {
    let row_ref = &mut editor_config.rows[row];
    if at > row_ref.chars.len() {
        at = row_ref.chars.len();
    }
    row_ref.chars.insert(at, c);
    editor_update_row(editor_config, row);
    editor_config.dirty = true;
}

fn editor_row_append_str(editor_config: &mut EditorConfig, row: usize, s: &str) {
    let row_ref = &mut editor_config.rows[row];
    row_ref.chars.push_str(s);
    editor_update_row(editor_config, row);
    editor_config.dirty = true;
}

fn editor_row_del_char(editor_config: &mut EditorConfig, row: usize, at: usize) {
    let row_ref = &mut editor_config.rows[row];
    if at >= row_ref.chars.len() {
        return;
    }
    row_ref.chars.remove(at);
    editor_update_row(editor_config, row);
    editor_config.dirty = true;
}

/*** editor operations ***/

fn editor_insert_char(editor_config: &mut EditorConfig, c: char) {
    if editor_config.cursor_y == editor_config.rows.len() {
        editor_insert_row(editor_config, editor_config.rows.len(), "");
    }
    editor_row_insert_char(
        editor_config,
        editor_config.cursor_y,
        editor_config.cursor_x,
        c,
    );
    editor_config.cursor_x += 1;
}

fn editor_insert_new_line(editor_config: &mut EditorConfig) {
    if editor_config.cursor_x == 0 {
        editor_insert_row(editor_config, editor_config.cursor_y, "");
    } else {
        let end = editor_config.rows[editor_config.cursor_y]
            .chars
            .split_off(editor_config.cursor_x);
        editor_update_row(editor_config, editor_config.cursor_y);
        editor_insert_row(editor_config, editor_config.cursor_y + 1, &end);
    }
    editor_config.cursor_y += 1;
    editor_config.cursor_x = 0;
}

fn editor_del_char(editor_config: &mut EditorConfig) {
    if editor_config.cursor_y == editor_config.rows.len() {
        return;
    }
    if editor_config.cursor_x == 0 && editor_config.cursor_y == 0 {
        return;
    }

    if editor_config.cursor_x > 0 {
        editor_row_del_char(
            editor_config,
            editor_config.cursor_y,
            editor_config.cursor_x - 1,
        );
        editor_config.cursor_x -= 1;
    } else {
        editor_config.cursor_x = editor_config.rows[editor_config.cursor_y - 1].chars.len();
        let deleted_row = editor_del_row(editor_config, editor_config.cursor_y).unwrap();
        editor_config.cursor_y -= 1;
        editor_row_append_str(editor_config, editor_config.cursor_y, &deleted_row.chars);
    }
}

/*** file i/o ***/

fn editor_rows_to_string(editor_config: &EditorConfig) -> String {
    let total_len = editor_config
        .rows
        .iter()
        .fold(0, |acc, row| acc + row.chars.len() + 1);

    let mut buf = String::with_capacity(total_len);
    for row in &editor_config.rows {
        buf.push_str(&row.chars);
        buf.push('\n');
    }

    buf
}

fn editor_open(editor_config: &mut EditorConfig, path: &str) -> Result<(), KiloError> {
    let contents = std::fs::read_to_string(path)?;

    editor_config.rows = contents
        .lines()
        .enumerate()
        .map(|(i, line)| EditorRow {
            idx: i,
            chars: line.to_owned(),
            render: String::with_capacity(0),
            hl: vec![],
            hl_open_comment: false,
        })
        .collect();

    editor_config.filename = Some(path.to_owned());
    editor_select_syntax_highlight(editor_config);
    for row in 0..editor_config.rows.len() {
        editor_update_row(editor_config, row);
    }
    editor_config.dirty = false;

    Ok(())
}

fn editor_save(editor_config: &mut EditorConfig) -> Result<(), KiloError> {
    if editor_config.filename.is_none() {
        match editor_prompt(
            editor_config,
            "Save as: {} (ESC to cancel)",
            editor_prompt_null_callback,
            // None::<FnOnce<&mut EditorConfig, &str, &KeyEvent>>,
        )? {
            Some(s) => editor_config.filename = Some(s),
            None => {
                editor_set_status_message(editor_config, "Save aborted".to_owned());
                return Ok(());
            }
        }
        editor_select_syntax_highlight(editor_config);
    }

    let buf = editor_rows_to_string(editor_config);
    let path = editor_config.filename.clone().unwrap();
    match std::fs::write(path, &buf) {
        Ok(_) => {
            editor_config.dirty = false;
            editor_set_status_message(
                editor_config,
                format!("{} bytes written to disk", buf.len()),
            );
        }
        Err(e) => editor_set_status_message(editor_config, format!("Can't save! I/O errro: {e}")),
    }
    Ok(())
}

/*** find ***/

fn editor_find_callback(editor_config: &mut EditorConfig, query: &str, key: &KeyEvent) {
    if let Some(saved_hl) = editor_config.find.saved_hl.take() {
        editor_config.rows[editor_config.find.saved_hl_line].hl = saved_hl;
    }

    match key {
        KeyEvent::Ctrl(chr) if *chr == '\r' as u8 => {
            editor_config.find = Default::default();
            return;
        }
        KeyEvent::Escape => {
            editor_config.find = Default::default();
            return;
        }
        KeyEvent::MoveCursor(dir) if [Direction::Right, Direction::Down].contains(dir) => {
            editor_config.find.direction = FindDirection::Forward
        }
        KeyEvent::MoveCursor(dir) if [Direction::Left, Direction::Up].contains(dir) => {
            editor_config.find.direction = FindDirection::Backward
        }
        _ => {
            editor_config.find = Default::default();
        }
    }

    if editor_config.find.last_match.is_none() {
        editor_config.find.direction = FindDirection::Forward;
    }
    let mut current = editor_config.find.last_match.map_or(-1, |i| i as isize);
    for _ in 0..editor_config.rows.len() {
        current += match editor_config.find.direction {
            FindDirection::Forward => 1,
            FindDirection::Backward => -1,
        };
        if current == -1 {
            current = editor_config.rows.len() as isize - 1;
        } else if current == editor_config.rows.len() as isize {
            current = 0;
        }
        let num_rows = editor_config.rows.len();
        let row = &mut editor_config.rows[current as usize];
        match row.render.find(&query) {
            Some(mtch) => {
                editor_config.find.last_match = Some(current as usize);
                editor_config.cursor_y = current as usize;
                editor_config.cursor_x = editor_row_render_x_to_cursor_x(row, mtch);
                editor_config.row_offset = num_rows;

                editor_config.find.saved_hl_line = current as usize;
                editor_config.find.saved_hl = Some(row.hl.clone());
                row.hl[mtch..mtch + query.len()].fill(EditorHighlight::Match);

                break;
            }
            _ => (),
        }
    }
}

fn editor_find(editor_config: &mut EditorConfig) -> Result<(), KiloError> {
    let saved_cursor_x = editor_config.cursor_x;
    let saved_cursor_y = editor_config.cursor_y;
    let saved_col_offset = editor_config.col_offset;
    let saved_row_offset = editor_config.row_offset;

    match editor_prompt(
        editor_config,
        "Search: {} (Use ESC/Arrows/Enter)",
        editor_find_callback,
    )? {
        Some(_) => (),
        None => {
            editor_config.cursor_x = saved_cursor_x;
            editor_config.cursor_y = saved_cursor_y;
            editor_config.col_offset = saved_col_offset;
            editor_config.row_offset = saved_row_offset;
        }
    };

    Ok(())
}

/*** output ***/

fn editor_scroll(editor_config: &mut EditorConfig) {
    editor_config.render_x = 0;
    if editor_config.cursor_y < editor_config.rows.len() {
        editor_config.render_x = editor_row_cursor_x_to_render_x(
            &editor_config.rows[editor_config.cursor_y],
            editor_config.cursor_x,
        );
    }

    if editor_config.cursor_y < editor_config.row_offset {
        editor_config.row_offset = editor_config.cursor_y;
    }
    if editor_config.cursor_y >= editor_config.row_offset + editor_config.screen_rows {
        editor_config.row_offset = editor_config.cursor_y - editor_config.screen_rows + 1;
    }
    if editor_config.render_x < editor_config.col_offset {
        editor_config.col_offset = editor_config.render_x;
    }
    if editor_config.render_x >= editor_config.col_offset + editor_config.screen_cols {
        editor_config.col_offset = editor_config.render_x - editor_config.screen_cols + 1;
    }
}

fn editor_draw_rows(editor_config: &EditorConfig, buf: &mut String) {
    for y in 0..editor_config.screen_rows {
        let file_row = y + editor_config.row_offset;
        if file_row >= editor_config.rows.len() {
            if editor_config.rows.len() == 0 && y == editor_config.screen_rows / 3 {
                let welcome = format!(
                    "{} editor -- version {}",
                    env!("CARGO_PKG_NAME"),
                    env!("CARGO_PKG_VERSION"),
                );
                let len = editor_config.screen_cols.min(welcome.len());
                let mut padding = (editor_config.screen_cols - len) / 2;
                if padding > 0 {
                    buf.push('~');
                    padding -= 1;
                }
                for _ in 0..padding {
                    buf.push(' ');
                }
                buf.push_str(&welcome[0..len]);
            } else {
                buf.push_str("~");
            }
        } else {
            let row = &editor_config.rows[file_row];
            let len = editor_config.rows[file_row]
                .render
                .len()
                .saturating_sub(editor_config.col_offset)
                .min(editor_config.screen_cols);
            let (from, to) = if row.render.len() > editor_config.col_offset {
                (editor_config.col_offset, editor_config.col_offset + len)
            } else {
                (0, 0)
            };
            let mut current_color = EditorHighlight::Normal;
            buf.push_str("\x1b[39m");
            for (i, c) in editor_config.rows[file_row]
                .render
                .char_indices()
                .skip(from)
                .take(to - from)
            {
                match &row.hl[i] {
                    EditorHighlight::Normal => {
                        if current_color != EditorHighlight::Normal {
                            buf.push_str("\x1b[39m");
                            current_color = EditorHighlight::Normal;
                        }
                        buf.push_str("\x1b[39m");
                        buf.push(c);
                    }
                    hl => {
                        let color = *hl;
                        if color != current_color {
                            buf.push_str(&format!("\x1b[{}m", editor_syntax_to_color(*hl)));
                        }
                        buf.push(c);
                    }
                }
            }
            buf.push_str("\x1b[39m");
        }

        buf.push_str("\x1b[K"); // clear line
        buf.push_str("\r\n");
    }
}

fn editor_draw_status_bar(editor_config: &EditorConfig, buf: &mut String) {
    buf.push_str("\x1b[7m");

    let filename = editor_config
        .filename
        .clone()
        .unwrap_or("[No Name]".to_owned());
    let modified = if editor_config.dirty {
        "(modified)".to_string()
    } else {
        String::with_capacity(0)
    };
    let status = format!(
        "{:.20} - {} lines {}",
        &filename,
        editor_config.rows.len(),
        modified
    );
    let rstatus = format!(
        "{} | {}/{}",
        if let Some(syntax) = &editor_config.syntax {
            syntax.file_type
        } else {
            "no ft"
        },
        editor_config.cursor_y + 1,
        editor_config.rows.len()
    );

    let mut len = status.len().min(editor_config.screen_cols);
    buf.push_str(&status[0..len]);

    while len < editor_config.screen_cols {
        if editor_config.screen_cols - len == rstatus.len() {
            buf.push_str(&rstatus);
            break;
        } else {
            buf.push(' ');
            len += 1;
        }
    }

    buf.push_str("\x1b[m");
    buf.push_str("\r\n");
}

fn editor_draw_message_bar(editor_config: &EditorConfig, buf: &mut String) {
    buf.push_str("\x1b[K");
    let len = editor_config
        .status_msg
        .len()
        .min(editor_config.screen_cols);
    let age = (Instant::now() - editor_config.status_msg_time).as_secs();
    if age < 5 {
        buf.push_str(&editor_config.status_msg[0..len]);
    }
}

fn editor_position_cursor(buf: &mut String, x: usize, y: usize) {
    buf.push_str(&format!("\x1b[{y};{x}H"));
}

fn editor_refresh_screen(editor_config: &mut EditorConfig) -> Result<(), KiloError> {
    editor_scroll(editor_config);

    let mut stdout = std::io::stdout();
    let mut buf = String::new();

    buf.push_str("\x1b[?25l"); // hide cursor

    editor_position_cursor(&mut buf, 1, 1);
    editor_draw_rows(editor_config, &mut buf);
    editor_draw_status_bar(editor_config, &mut buf);
    editor_draw_message_bar(editor_config, &mut buf);
    editor_position_cursor(
        &mut buf,
        editor_config.render_x - editor_config.col_offset + 1,
        editor_config.cursor_y - editor_config.row_offset + 1,
    );

    buf.push_str("\x1b[?25h"); // show cursor

    stdout.write(buf.as_bytes())?;
    stdout.flush()?;
    Ok(())
}

fn editor_set_status_message(editor_config: &mut EditorConfig, msg: String) {
    editor_config.status_msg = msg;
    editor_config.status_msg_time = Instant::now();
}

/*** input ***/

fn editor_prompt_null_callback(_: &mut EditorConfig, _: &str, _: &KeyEvent) {
    //
}

fn editor_prompt<F>(
    editor_config: &mut EditorConfig,
    prompt: &str,
    callback: F,
) -> Result<Option<String>, KiloError>
where
    F: Fn(&mut EditorConfig, &str, &KeyEvent),
{
    let mut buf = String::new();

    loop {
        editor_set_status_message(editor_config, prompt.replace("{}", &buf));
        editor_refresh_screen(editor_config)?;

        let c = editor_read_key()?;

        match c {
            KeyEvent::Escape => {
                editor_set_status_message(editor_config, "".to_owned());
                callback(editor_config, &buf, &c);
                return Ok(None);
            }
            KeyEvent::Ctrl(chr) if chr == '\r' as u8 => {
                if !buf.is_empty() {
                    editor_set_status_message(editor_config, "".to_owned());
                    callback(editor_config, &buf, &c);
                    return Ok(Some(buf));
                }
            }
            KeyEvent::Backspace => {
                buf.pop();
            }
            KeyEvent::Key(c) => {
                buf.push(c);
            }
            _ => (),
        }

        callback(editor_config, &buf, &c);
    }
}

#[derive(PartialEq)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

fn editor_move_cursor(editor_config: &mut EditorConfig, dir: Direction, n: usize) {
    for _ in 0..n {
        let cur_row = if editor_config.cursor_y >= editor_config.rows.len() {
            None
        } else {
            Some(&editor_config.rows[editor_config.cursor_y])
        };

        match dir {
            Direction::Left => {
                if editor_config.cursor_x != 0 {
                    editor_config.cursor_x = editor_config.cursor_x.saturating_sub(1);
                } else if editor_config.cursor_y > 0 {
                    editor_config.cursor_y -= 1;
                    editor_config.cursor_x = editor_config.rows[editor_config.cursor_y].chars.len();
                }
            }
            Direction::Right => {
                if let Some(cur_row) = cur_row {
                    if editor_config.cursor_x < cur_row.chars.len() {
                        editor_config.cursor_x += 1;
                    } else if editor_config.cursor_x == cur_row.chars.len() {
                        editor_config.cursor_y += 1;
                        editor_config.cursor_x = 0;
                    }
                }
            }
            Direction::Up => editor_config.cursor_y = editor_config.cursor_y.saturating_sub(1),
            Direction::Down => {
                if editor_config.cursor_y < editor_config.rows.len() {
                    editor_config.cursor_y += 1;
                }
            }
        }
    }

    let cur_row = if editor_config.cursor_y >= editor_config.rows.len() {
        None
    } else {
        Some(&editor_config.rows[editor_config.cursor_y])
    };
    let row_len = match cur_row {
        Some(cur_row) => cur_row.chars.len(),
        None => 0,
    };
    if editor_config.cursor_x > row_len {
        editor_config.cursor_x = row_len;
    }
}

enum KeyEvent {
    Escape,
    Key(char),
    Ctrl(u8),
    MoveCursor(Direction),
    DeleteKey,
    Backspace,
    HomeKey,
    EndKey,
    PageUp,
    PageDown,
}

fn editor_process_keypress() -> Result<KeyEvent, KiloError> {
    editor_read_key()
}

/*** init ***/

fn init_editor(editor_config: &EditorConfig) -> Result<(usize, usize), KiloError> {
    get_window_size(editor_config)
}

fn main() {
    let orig_termios = enable_raw_mode();
    let mut editor_config = EditorConfig {
        cursor_x: 0,
        cursor_y: 0,
        render_x: 0,
        row_offset: 0,
        col_offset: 0,
        screen_rows: 0,
        screen_cols: 0,
        rows: vec![],
        dirty: false,
        filename: None,
        status_msg: String::with_capacity(0),
        status_msg_time: Instant::now(),
        syntax: None,
        orig_termios,
        quit_times: KILO_QUIT_TIMES,
        find: Default::default(),
    };

    let (screen_rows, screen_cols) =
        init_editor(&editor_config).unwrap_or_else(|e| die(Some(&mut editor_config), e));
    editor_config.screen_rows = screen_rows - 2;
    editor_config.screen_cols = screen_cols;

    match kilo_main(&mut editor_config) {
        Ok(_) => (),
        Err(e) => die(Some(&mut editor_config), e),
    }

    disable_raw_mode(&editor_config);
}

fn kilo_main(editor_config: &mut EditorConfig) -> Result<(), KiloError> {
    let args = std::env::args().collect::<Vec<String>>();
    if args.len() >= 2 {
        editor_open(editor_config, &args[1])?;
    }

    editor_set_status_message(
        editor_config,
        "HELP: Ctrl-S = save | Ctrl-Q = quit | Ctrl-F = find".to_owned(),
    );

    loop {
        editor_refresh_screen(editor_config)?;
        match editor_process_keypress()? {
            KeyEvent::Ctrl(c) if c == ctrl_key('q') => {
                if editor_config.dirty && editor_config.quit_times > 0 {
                    editor_set_status_message(editor_config, format!("WARNING!!! File has unsaved changes. Press Ctrl-Q {} more times to quit", editor_config.quit_times));
                    editor_config.quit_times -= 1;
                    continue;
                }
                break;
            }
            KeyEvent::Ctrl(c) if c == ctrl_key('s') => editor_save(editor_config)?,
            KeyEvent::Ctrl(c) if c == ctrl_key('f') => editor_find(editor_config)?,
            KeyEvent::Ctrl(c) if c == ctrl_key('h') => (),
            KeyEvent::Ctrl(c) if c == ctrl_key('l') => (),
            KeyEvent::Ctrl(c) if c == '\r' as u8 => editor_insert_new_line(editor_config),
            KeyEvent::Ctrl(c) if c == '\t' as u8 => {
                for _ in 0..KILO_TAB_STOP {
                    editor_insert_char(editor_config, ' ');
                }
            }
            KeyEvent::Ctrl(ctrl) => {
                editor_set_status_message(editor_config, format!("Ctrl({})", ctrl))
            }
            KeyEvent::Escape => (),
            KeyEvent::Backspace => editor_del_char(editor_config),
            KeyEvent::DeleteKey => {
                editor_move_cursor(editor_config, Direction::Right, 1);
                editor_del_char(editor_config);
            }
            KeyEvent::HomeKey => editor_config.cursor_x = 0,
            KeyEvent::EndKey => {
                if editor_config.cursor_y <= editor_config.rows.len() {
                    let len = editor_config.rows[editor_config.cursor_y].chars.len()
                        - editor_config.cursor_x;
                    editor_move_cursor(editor_config, Direction::Right, len);
                }
            }
            KeyEvent::PageUp => {
                editor_move_cursor(editor_config, Direction::Up, editor_config.screen_rows)
            }
            KeyEvent::PageDown => {
                editor_move_cursor(editor_config, Direction::Down, editor_config.screen_rows)
            }
            KeyEvent::MoveCursor(dir) => editor_move_cursor(editor_config, dir, 1),
            KeyEvent::Key(c) => {
                editor_insert_char(editor_config, c);
            }
        }
        editor_config.quit_times = KILO_QUIT_TIMES;
    }

    editor_refresh_screen(editor_config)?;
    Ok(())
}

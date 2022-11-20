use emacs::{Env, Result, Value, defun};
use cjk::{is_cjk_codepoint, is_cjk_punctuation_codepoint};

emacs::plugin_is_GPL_compatible!();

#[emacs::module]
fn init(_: &Env) -> Result<()> {
    Ok(())
}

#[defun]
fn do_count(env: &Env, content: String) -> Result<Value<'_>> {
    let mut chinese = 0;
    let mut punc = 0;

    for c in content.chars() {
        if c.is_whitespace() {
            continue;
        }

        if is_cjk_codepoint(c) {
            if is_cjk_punctuation_codepoint(c) {
                punc += 1;
            } else {
                chinese += 1;
            }
        }
    }

    env.message(&format!("there are {} chinese, {} punc.", chinese, punc))
}

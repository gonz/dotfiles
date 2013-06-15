set -gx PATH ~/Bin ~/.rbenv/bin ~/.rbenv/shims $PATH
set -gx LC_ALL en_US.UTF-8
set -gx CDPATH .

# Virtualenv
set -gx VIRTUALENVS_PATH $HOME/.virtualenvs/
. ~/.config/fish/virtualenv.fish

# Postgres
set -gx PGHOST localhost

# Prismatical
set -gx PRISM_SETTINGS $HOME/prismatical/prism/config/settings.py

# Prompt (user, host, cwd, git branch/status)
function fish_prompt
    set prompt (set_color green)(whoami)(set_color black)'@'(set_color yellow)(hostname|cut -d . -f 1)(set_color black)':'(set_color blue)(pwd)
    # git (current branch, status)
    if [ -d .git ]
        # branch
        set branch (git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \([^ ]*\)/\1/')
        if [ -z $branch ]
          set branch '__init__'
        end
        set prompt $prompt(set_color black)' git:'(set_color yellow)$branch
        # status
        git diff --quiet HEAD ^&-
        if [ $status = 1 ]
            set prompt $prompt(set_color red)♺
        else
            set prompt $prompt(set_color green)✓
        end
    end
    # virtualenv (current env)
    if  [ $VIRTUAL_ENV ]
        set prompt $prompt(set_color black)' env:'(set_color yellow)(basename "$VIRTUAL_ENV")
    end
    # cherry
    echo $prompt(set_color magenta)' > '
end

# Disable welcome msg
set fish_greeting ""

# Enable autojump
[ -s ~/.autojump/etc/profile.d/autojump.fish ]; and . ~/.autojump/etc/profile.d/autojump.fish

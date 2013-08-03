function mkvirtualenv -d "Create a new python 2.7 virtual environment in $VIRTUALENVS_PATH"
  set python_bin /usr/bin/python2.7
  set tgt {$VIRTUALENVS_PATH}$argv[1]

  if [ -d $tgt ]
    echo "$tgt already exists"
  else
    virtualenv --no-site-packages --distribute -p $python_bin $tgt
  end
end

function workon -d "Activate virtual environment in $VIRTUALENVS_PATH"
  set tgt {$VIRTUALENVS_PATH}$argv[1]
  echo $tgt
  if [ -d $tgt ]
    deactivate

    set -gx VIRTUAL_ENV "$tgt"
    set -gx _OLD_VIRTUAL_PATH $PATH
    set -gx PATH "$VIRTUAL_ENV/bin" $PATH

    # unset PYTHONHOME if set
    if set -q PYTHONHOME
       set -gx _OLD_VIRTUAL_PYTHONHOME $PYTHONHOME
       set -e PYTHONHOME
    end
  else
    echo "$tgt not found"
  end
end
complete -c workon -a "(pushd $VIRTUALENVS_PATH VIRTUALENVS_PATH; ls -d *; popd)"


function deactivate -d "Exit virtualenv and return to normal shell environment"
    # reset old environment variables
    if test -n "$_OLD_VIRTUAL_PATH"
        set -gx PATH $_OLD_VIRTUAL_PATH
        set -e _OLD_VIRTUAL_PATH
    end
    if test -n "$_OLD_VIRTUAL_PYTHONHOME"
        set -gx PYTHONHOME $_OLD_VIRTUAL_PYTHONHOME
        set -e _OLD_VIRTUAL_PYTHONHOME
    end
    set -e VIRTUAL_ENV
end

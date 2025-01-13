#!/bin/bash

# 检查是否提供了参数
if [ $# -eq 0 ]; then
    echo "Usage: $0 {update|install}"
    exit 1
fi

# 根据参数执行不同的命令
case $1 in
    update)
        echo "Running update command..."
        echo "update submodule"
        git submodule foreach git pull &&
        echo "update package" &&
        emacs --batch -l base.el --eval "(package-upgrade-all)" &&
        emacs --batch -l base.el --eval "(package-vc-upgrade-all)" &&
	    # emacs --batch -l base.el --eval "(package-recompile-all)"
        echo "Emacs update finished."
        ;;
    install)
        echo "Running install command..."
        emacs --batch -l base.el --eval "(install-all-packages)" &&
        emacs --batch -l base.el --eval "(package-recompile-all)" &&
        echo "Emacs install finished."
        ;;
    recompile)
        echo "Running install command..."
        emacs --batch -l base.el --eval "(package-recompile-all)" &&
        echo "Emacs recompile finished."
        ;;
    *)
        echo "Invalid command: $1"
        echo "Usage: $0 {update|install}"
        exit 1
        ;;
esac

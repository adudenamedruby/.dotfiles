#!/bin/bash

echo "+++ Stowing dotfiles +++"
cd ~/.dotfiles
stow --restow */

#!/bin/sh

installThemes() {
    echo "This utility will install themes for Xcode. Do you want to contiue (y/n)"
    read resp
	  if [ "$resp" = 'y' -o "$resp" = 'Y' ] ; then
		    echo "Copying themes."
        cp ~/.dotfiles/Themes/* ~/Library/Developer/Xcode/UserData/FontAndColorThemes/
	  else
		    echo "Aborting theme copying."
	  fi

}

installThemes

#!/bin/zsh --no-rcs

# Get filepath for Default Profile
case "${releaseChannel}" in
	"zen")
		readonly versionCode="6ED35B3CA1B5D3AF"
		;;
	"zenTwilight")
		readonly versionCode="9EBD2AC824310766"
		;;
esac
readonly defaultProfile=$(awk -v versionCode="$versionCode" 'BEGIN {FS="="} $0 ~ versionCode {flag=1} flag && /^Default=Profiles/ {print $2; exit}' "${HOME}/Library/Application Support/zen/installs.ini")
defaultProfilePath="/Library/Application Support/zen/${defaultProfile}"

if [[ -z ${defaultProfile} ]]; then
	defaultProfileSubtext="❌ No Profiles Found in ~/Library/Application Support/zen ❌"
	defaultProfileArg="${HOME}/Library/Application Support/zen"
else
	defaultProfileSubtext="~${defaultProfilePath}"
	defaultProfileArg="${HOME}/${defaultProfilePath}"
fi

cat << EOB
{"items": [
	{
		"title": "Open Zen Profile Manager",
		"icon": { "path": "images/${releaseChannel}Logo.png" },
		"variables": { "pref_id": "profileManager" }
	},
	{
		"title": "Open Default Profile in Finder",
		"subtitle": "${defaultProfileSubtext}",
		"arg": "file://${defaultProfileArg}",
		"icon": { "path": "images/${releaseChannel}Logo.png" },
		"variables": { "pref_id": "profilePath" }
	},
	{
	    "title": "Configure Workflow...",
	    "subtitle": "Open the configuration window for ${alfred_workflow_name}",
		"arg": "alfredpreferences://navigateto/workflows>workflow>${alfred_workflow_uid}>userconfig",
		"icon": { "path": "images/${releaseChannel}Logo.png" },
		"variables": { "pref_id": "configure" }
	},
]}
EOB
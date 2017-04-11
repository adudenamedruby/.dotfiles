#################################
#
#        Build script for Janus Static Library
#
#   This script will automagically build a cross-platform static library from
#   Objective-C code.
#
#   - This script was cobbled together from several StackOverflow threads and
#   may have a few frivolous commands that have escaped me.
#
#################################

# Setup
set -e
set -o pipefail

# Step 1: Find the BASESDK version number
SDK_VERSION=$(echo ${SDK_NAME} | grep -o '\d\{1,2\}\.\d\{1,2\}$')

# Step 2: Figure out if we're in SIM or DEVICE build
if [ ${PLATFORM_NAME} = "iphonesimulator" ]
then
OTHER_SDK_TO_BUILD=iphoneos${SDK_VERSION}
else
OTHER_SDK_TO_BUILD=iphonesimulator${SDK_VERSION}
fi

# Xcode is already building ONE target but beacuse it's a library, Apple wants to
# only build one, which is not what we want. A static library requires all targets.
# But we MUST NOT re-build the target that is ALREADY being built or Xcode WILL
# CRASH YOUR COMPUTER if you try this (infinite recursion!)

# Step 3: Build ONLY the missing platforms/configurations.
if [ "true" == ${ALREADYINVOKED:-false} ]
then
echo "Running..."
else
# CRITICAL:
# Prevent infinite recursion
export ALREADYINVOKED="true"

xcodebuild -configuration "${CONFIGURATION}" -project "${PROJECT_NAME}.xcodeproj" -target "${TARGET_NAME}" -sdk "${OTHER_SDK_TO_BUILD}" ${ACTION} RUN_CLANG_STATIC_ANALYZER=NO BUILD_DIR="${BUILD_DIR}" BUILD_ROOT="${BUILD_ROOT}" SYMROOT="${SYMROOT}"

ACTION="build"

# Step 4: Determine where the built files are coming from and where they are going:
CURRENTCONFIG_DEVICE_DIR=${SYMROOT}/${CONFIGURATION}-iphoneos
CURRENTCONFIG_SIMULATOR_DIR=${SYMROOT}/${CONFIGURATION}-iphonesimulator
UNIVERSAL_DESTINATION_DIR=~/Desktop/Janus\ Universal/

# Also, remove the products of previous runs of this script
rm -rf "${UNIVERSAL_DESTINATION_DIR}"
mkdir "${UNIVERSAL_DESTINATION_DIR}"

# Step 5: Run the lipo tool
xcrun -sdk iphoneos lipo -create -output "${UNIVERSAL_DESTINATION_DIR}/${EXECUTABLE_NAME}" "${CURRENTCONFIG_DEVICE_DIR}/${EXECUTABLE_NAME}" "${CURRENTCONFIG_SIMULATOR_DIR}/${EXECUTABLE_NAME}"

# Step 6: Um, Just for kicks, let's also get the header files into our new directory.
# Sometimes this fails, so you should just build again.
if [ -d "${CURRENTCONFIG_DEVICE_DIR}/usr/local/include/" ]
then
cp -r "${CURRENTCONFIG_DEVICE_DIR}/usr/local/include/"* "${UNIVERSAL_DESTINATION_DIR}"
fi

# Step 7: For convenience, open the new directory, too!
open "${UNIVERSAL_DESTINATION_DIR}"

fi
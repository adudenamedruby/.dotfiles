// -*- mode: c++ -*-
// Copyright 2016 Keyboardio, inc. <jesse@keyboard.io>
// See "LICENSE" for license details

#ifndef BUILD_INFORMATION
#define BUILD_INFORMATION "locally built"
#endif


/**
 * These #include directives pull in the Kaleidoscope firmware core,
 * as well as the Kaleidoscope plugins we use in the Model 01's firmware
 */


// The Kaleidoscope core
#include "Kaleidoscope.h"

// Support for storing the keymap in EEPROM
#include "Kaleidoscope-EEPROM-Settings.h"
#include "Kaleidoscope-EEPROM-Keymap.h"

// Support for communicating with the host via a simple Serial protocol
#include "Kaleidoscope-FocusSerial.h"

// Support for keys that move the mouse
#include "Kaleidoscope-MouseKeys.h"

// Support for macros
#include "Kaleidoscope-Macros.h"

// Support for controlling the keyboard's LEDs
#include "Kaleidoscope-LEDControl.h"

// Support for "Numpad" mode, which is mostly just the Numpad specific LED mode
#include "Kaleidoscope-NumPad.h"

// Support for the "Boot greeting" effect, which pulses the 'LED' button for 10s
// when the keyboard is connected to a computer (or that computer is powered on)
#include "Kaleidoscope-LEDEffect-BootGreeting.h"

// Support for LED modes that set all LEDs to a single color
#include "Kaleidoscope-LEDEffect-SolidColor.h"

// Support for an LED mode that makes all the LEDs 'breathe'
#include "Kaleidoscope-LEDEffect-Breathe.h"

// Support for an LED mode that makes a red pixel chase a blue pixel across the keyboard
#include "Kaleidoscope-LEDEffect-Chase.h"

// Support for LED modes that pulse the keyboard's LED in a rainbow pattern
#include "Kaleidoscope-LEDEffect-Rainbow.h"

// Support for an LED mode that lights up the keys as you press them
#include "Kaleidoscope-LED-Stalker.h"

// Support for DigitalRain LED
//#include <Kaleidoscope-LEDEffect-DigitalRain.h>

// Support for Layer highlighter
//#include "Kaleidoscope-LayerHighlighter.h"

// Support for TapDance
#include "Kaleidoscope-TapDance.h"

// Support for an LED mode that prints the keys you press in letters 4px high
#include "Kaleidoscope-LED-AlphaSquare.h"

// Support for Keyboardio's internal keyboard testing mode
#include "Kaleidoscope-Model01-TestMode.h"

// Support for host power management (suspend & wakeup)
#include "Kaleidoscope-HostPowerManagement.h"

// Support for magic combos (key chords that trigger an action)
#include "Kaleidoscope-MagicCombo.h"

// Support for USB quirks, like changing the key state report protocol
#include "Kaleidoscope-USB-Quirks.h"

// Support for Qukeys, enabling tap and hold behaviour
#include "Kaleidoscope-Qukeys.h"



/** This 'enum' is a list of all the macros used by the Model 01's firmware
  * The names aren't particularly important. What is important is that each
  * is unique.
  *
  * These are the names of your macros. They'll be used in two places.
  * The first is in your keymap definitions. There, you'll use the syntax
  * `M(MACRO_NAME)` to mark a specific keymap position as triggering `MACRO_NAME`
  *
  * The second usage is in the 'switch' statement in the `macroAction` function.
  * That switch statement actually runs the code associated with a macro when
  * a macro key is pressed.
  */

enum { MACRO_VERSION_INFO,
       MACRO_ANY
};

/** The Model 01's key layouts are defined as 'keymaps'. By default, there are three
  * keymaps: The standard QWERTY keymap, the "Function layer" keymap and the "Numpad"
  * keymap.
  *
  * Each keymap is defined as a list using the 'KEYMAP_STACKED' macro, built
  * of first the left hand's layout, followed by the right hand's layout.
  *
  * Keymaps typically consist mostly of `Key_` definitions. There are many, many keys
  * defined as part of the USB HID Keyboard specification. You can find the names
  * (if not yet the explanations) for all the standard `Key_` defintions offered by
  * Kaleidoscope in these files:
  *    https://github.com/keyboardio/Kaleidoscope/blob/master/src/key_defs_keyboard.h
  *    https://github.com/keyboardio/Kaleidoscope/blob/master/src/key_defs_consumerctl.h
  *    https://github.com/keyboardio/Kaleidoscope/blob/master/src/key_defs_sysctl.h
  *    https://github.com/keyboardio/Kaleidoscope/blob/master/src/key_defs_keymaps.h
  *
  * Additional things that should be documented here include
  *   using ___ to let keypresses fall through to the previously active layer
  *   using XXX to mark a keyswitch as 'blocked' on this layer
  *   using ShiftToLayer() and LockLayer() keys to change the active keymap.
  *   keeping NUM and FN consistent and accessible on all layers
  *
  * The PROG key is special, since it is how you indicate to the board that you
  * want to flash the firmware. However, it can be remapped to a regular key.
  * When the keyboard boots, it first looks to see whether the PROG key is held
  * down; if it is, it simply awaits further flashing instructions. If it is
  * not, it continues loading the rest of the firmware and the keyboard
  * functions normally, with whatever binding you have set to PROG. More detail
  * here: https://community.keyboard.io/t/how-the-prog-key-gets-you-into-the-bootloader/506/8
  *
  * The "keymaps" data structure is a list of the keymaps compiled into the firmware.
  * The order of keymaps in the list is important, as the ShiftToLayer(#) and LockLayer(#)
  * macros switch to key layers based on this list.
  *
  *

  * A key defined as 'ShiftToLayer(FUNCTION)' will switch to FUNCTION while held.
  * Similarly, a key defined as 'LockLayer(NUMPAD)' will switch to NUMPAD when tapped.
  */

/**
  * Layers are "0-indexed" -- That is the first one is layer 0. The second one is layer 1.
  * The third one is layer 2.
  * This 'enum' lets us use names like QWERTY, FUNCTION, and NUMPAD in place of
  * the numbers 0, 1 and 2.
  *
  */

enum { PRIMARY, NUMBERS_FUNCTION, PROGRAMMING_CHARACTERS, ARROWS, HYPER, MEH, MEDIA, DVORAK, WOW }; // layers


/* This comment temporarily turns off astyle's indent enforcement
 *   so we can make the keymaps actually resemble the physical key layout better
 */
// *INDENT-OFF*

KEYMAPS(

  [PRIMARY] = KEYMAP_STACKED
  (___,                         Key_1,           Key_2,          Key_3,           Key_4,       Key_5,       Key_LEDEffectNext,
   Key_Tab,                     Key_Q,           Key_W,          Key_E,           Key_R,       Key_T,       LGUI(Key_I),
   Key_CapsLock,                Key_A,           Key_S,          Key_D,           Key_F,       Key_G,
   Key_LeftShift,               Key_Z,           Key_X,          Key_C,           Key_V,       Key_B,       LGUI(Key_Period),
   Key_LeftGui, Key_Spacebar, Key_Enter, ALT_T(Tab),
   ShiftToLayer(NUMBERS_FUNCTION),

   M(MACRO_ANY),        Key_6,           Key_7,          Key_8,          Key_9,         Key_0,         LockLayer(DVORAK),
   ShiftToLayer(MEDIA), Key_Y,           Key_U,          Key_I,          Key_O,         Key_P,         LGUI(Key_Slash),
                        Key_H,           Key_J,          Key_K,          Key_L,         Key_Semicolon, Key_RightShift,
   LockLayer(WOW),      Key_N,           Key_M,          Key_Comma,      Key_Period,    Key_Slash,     LSHIFT(LGUI(Key_Slash)),
   Key_RightAlt, Key_LeftShift, Key_Backspace, CTL_T(Escape),
   ShiftToLayer(PROGRAMMING_CHARACTERS)),


  [NUMBERS_FUNCTION] =  KEYMAP_STACKED
  (___, Key_F6, Key_F7, Key_F8, Key_F9, Key_F10, ___,
   ___, Key_F1, Key_F2, Key_F3, Key_F4, Key_F5, ___,
   ___, ___, ___, ___, ___, ___,
   ___, ___, ___, ___, ___, ___, ___,
   ___, Key_Backspace, Key_LeftShift, ___,
   ___,

   ___, ___, ___,   ___,   ___,   ___,   ___,
   ___, ___, Key_7, Key_8, Key_9, ___,   ___,
        ___, Key_4, Key_5, Key_6, Key_0, ___,
   ___, ___, Key_1, Key_2, Key_3, ___, ___,
   ___, ___, ___, ___,
   ShiftToLayer(MEH)),


  [PROGRAMMING_CHARACTERS] =  KEYMAP_STACKED
  (___, ___, ___, ___, ___, ___, ___,
   ___, ___,                    Key_Backtick,               LSHIFT(Key_2),         LSHIFT(Key_5),      Key_Backslash,       ___,
   ___, Key_LeftBracket,        LSHIFT(Key_Minus),          LSHIFT(Key_Z),         LSHIFT(Key_9),      LSHIFT(Key_RightBracket),
   ___, LSHIFT(Key_Backtick),   Key_Minus,                  LSHIFT(Key_7),         LSHIFT(Key_LeftBracket),  ___,                 ___,
   ___, ___, ___, ___,
   ShiftToLayer(HYPER),

   ___, ___, ___, ___, ___, ___, ___,
   ___, LSHIFT(Key_Backslash), Key_0,                 LSHIFT(Key_6),              LSHIFT(Key_4),              ___,                   ___,
        LSHIFT(Key_8),         LSHIFT(Key_0),         Key_RightBracket,           LSHIFT(Key_Equals),         Key_Quote,             ___,
   ___, ___,                   LSHIFT(Key_1),         LSHIFT(Key_Quote),          Key_Equals,                 LSHIFT(Key_3),         ___,
   ___, ___, ShiftToLayer(ARROWS), ___,
   ___),


   [ARROWS] =  KEYMAP_STACKED
  (___, ___, ___, ___, ___, ___, ___,
   ___, ___, ___, ___, ___, ___, ___,
   ___, ___, ___, ___, ___, ___,
   ___, ___, ___, ___, ___, ___, ___,
   ___, ___, ___, ___,
   ___,
   
   ___, ___, ___,           ___,           ___,            ___, ___,
   ___, ___, ___,           Key_UpArrow,   ___,            ___, ___,
        ___, Key_LeftArrow, Key_DownArrow, Key_RightArrow, ___, ___,
   ___, ___, ___,           ___,           ___,            ___, ___,
   ___, ___, ___, ___,
   XXX),


   [HYPER] = KEYMAP_STACKED
  (___, LCTRL(LALT(LSHIFT(LGUI(Key_1)))),         LCTRL(LALT(LSHIFT(LGUI(Key_2)))),          LCTRL(LALT(LSHIFT(LGUI(Key_3)))),           LCTRL(LALT(LSHIFT(LGUI(Key_4)))), LCTRL(LALT(LSHIFT(LGUI(Key_5)))), XXX,
   XXX, LCTRL(LALT(LSHIFT(LGUI(Key_Q)))),         LCTRL(LALT(LSHIFT(LGUI(Key_W)))),          LCTRL(LALT(LSHIFT(LGUI(Key_E)))),           LCTRL(LALT(LSHIFT(LGUI(Key_R)))), LCTRL(LALT(LSHIFT(LGUI(Key_T)))), XXX,
   XXX, LCTRL(LALT(LSHIFT(LGUI(Key_A)))),         LCTRL(LALT(LSHIFT(LGUI(Key_S)))),          LCTRL(LALT(LSHIFT(LGUI(Key_D)))),           LCTRL(LALT(LSHIFT(LGUI(Key_F)))), LCTRL(LALT(LSHIFT(LGUI(Key_G)))),
   XXX, LCTRL(LALT(LSHIFT(LGUI(Key_Z)))),         LCTRL(LALT(LSHIFT(LGUI(Key_X)))),          LCTRL(LALT(LSHIFT(LGUI(Key_C)))),           LCTRL(LALT(LSHIFT(LGUI(Key_V)))), LCTRL(LALT(LSHIFT(LGUI(Key_B)))), XXX,
   XXX, XXX, XXX, XXX,
   ___,

   XXX, LCTRL(LALT(LSHIFT(LGUI(Key_6)))), LCTRL(LALT(LSHIFT(LGUI(Key_7)))), LCTRL(LALT(LSHIFT(LGUI(Key_8)))), LCTRL(LALT(LSHIFT(LGUI(Key_9)))), LCTRL(LALT(LSHIFT(LGUI(Key_0)))), XXX,
   XXX, LCTRL(LALT(LSHIFT(LGUI(Key_Y)))), LCTRL(LALT(LSHIFT(LGUI(Key_U)))), LCTRL(LALT(LSHIFT(LGUI(Key_I)))), LCTRL(LALT(LSHIFT(LGUI(Key_O)))), LCTRL(LALT(LSHIFT(LGUI(Key_P)))), XXX,
        LCTRL(LALT(LSHIFT(LGUI(Key_H)))), LCTRL(LALT(LSHIFT(LGUI(Key_J)))), LCTRL(LALT(LSHIFT(LGUI(Key_K)))), LCTRL(LALT(LSHIFT(LGUI(Key_L)))), LCTRL(LALT(LSHIFT(LGUI(Key_Semicolon)))), XXX,
   XXX, LCTRL(LALT(LSHIFT(LGUI(Key_N)))), LCTRL(LALT(LSHIFT(LGUI(Key_M)))), LCTRL(LALT(LSHIFT(LGUI(Key_Comma)))), LCTRL(LALT(LSHIFT(LGUI(Key_Period)))), LCTRL(LALT(LSHIFT(LGUI(Key_Slash)))), XXX,
   XXX, XXX, XXX, XXX,
   XXX),


   [MEH] = KEYMAP_STACKED
  (___, LCTRL(LALT(LSHIFT(Key_1))),         LCTRL(LALT(LSHIFT(Key_2))),          LCTRL(LALT(LSHIFT(Key_3))),           LCTRL(LALT(LSHIFT(Key_4))), LCTRL(LALT(LSHIFT(Key_5))), XXX,
   XXX, LCTRL(LALT(LSHIFT(Key_Q))),         LCTRL(LALT(LSHIFT(Key_W))),          LCTRL(LALT(LSHIFT(Key_E))),           LCTRL(LALT(LSHIFT(Key_R))), LCTRL(LALT(LSHIFT(Key_T))), XXX,
   XXX, LCTRL(LALT(LSHIFT(Key_A))),         LCTRL(LALT(LSHIFT(Key_S))),          LCTRL(LALT(LSHIFT(Key_D))),           LCTRL(LALT(LSHIFT(Key_F))), LCTRL(LALT(LSHIFT(Key_G))),
   XXX, LCTRL(LALT(LSHIFT(Key_Z))),         LCTRL(LALT(LSHIFT(Key_X))),          LCTRL(LALT(LSHIFT(Key_C))),           LCTRL(LALT(LSHIFT(Key_V))), LCTRL(LALT(LSHIFT(Key_B))), XXX,
   XXX, XXX, XXX, XXX,
   XXX,

   XXX, LCTRL(LALT(LSHIFT(Key_6))), LCTRL(LALT(LSHIFT(Key_7))), LCTRL(LALT(LSHIFT(Key_8))), LCTRL(LALT(LSHIFT(Key_9))), LCTRL(LALT(LSHIFT(Key_0))), XXX,
   XXX, LCTRL(LALT(LSHIFT(Key_Y))), LCTRL(LALT(LSHIFT(Key_U))), LCTRL(LALT(LSHIFT(Key_I))), LCTRL(LALT(LSHIFT(Key_O))), LCTRL(LALT(LSHIFT(Key_P))), XXX,
        LCTRL(LALT(LSHIFT(Key_H))), LCTRL(LALT(LSHIFT(Key_J))), LCTRL(LALT(LSHIFT(Key_K))), LCTRL(LALT(LSHIFT(Key_L))), LCTRL(LALT(LSHIFT(Key_Semicolon))), XXX,
   XXX, LCTRL(LALT(LSHIFT(Key_N))), LCTRL(LALT(LSHIFT(Key_M))), LCTRL(LALT(LSHIFT(Key_Comma))), LCTRL(LALT(LSHIFT(Key_Period))), LCTRL(LALT(LSHIFT(Key_Slash))), XXX,
   XXX, XXX, XXX, XXX,
   ___),


   [MEDIA] =  KEYMAP_STACKED
  (___, ___, ___, ___, ___, ___, ___,
   ___, ___, ___, ___, ___, ___, ___,
   ___, ___, ___, ___, ___, ___,
   ___, ___, ___, ___, ___, ___, ___,
   ___, ___, ___, ___,
   ___,
   
   ___,                        ___,                      ___, ___, ___, ___, ___,
   ___,                        Consumer_Mute,            ___, ___, ___, ___, ___,
                               Consumer_VolumeIncrement, ___, ___, ___, ___, ___,
   Consumer_PlaySlashPause,    Consumer_VolumeDecrement, ___, ___, ___, ___, ___,
   ___, ___, ___, ___,
   XXX),


   [DVORAK] = KEYMAP_STACKED
  (___,          Key_1,         Key_2,     Key_3,      Key_4, Key_5, Key_LEDEffectNext,
   Key_Backtick, Key_Quote,     Key_Comma, Key_Period, Key_P, Key_Y, Key_Tab,
   Key_PageUp,   Key_A,         Key_O,     Key_E,      Key_U, Key_I,
   Key_PageDown, Key_Semicolon, Key_Q,     Key_J,      Key_K, Key_X, Key_Escape,
   Key_LeftGui, Key_Spacebar, Key_Enter, Key_LeftControl,
   XXX,

   M(MACRO_ANY),   Key_6, Key_7, Key_8, Key_9, Key_0, ___,
   Key_Enter,      Key_F, Key_G, Key_C, Key_R, Key_L, Key_Slash,
                   Key_D, Key_H, Key_T, Key_N, Key_S, Key_Minus,
   Key_RightAlt,   Key_B, Key_M, Key_W, Key_V, Key_Z, Key_Equals,
   Key_RightAlt, Key_LeftShift, Key_Backspace, Key_RightControl,
   XXX),

   [WOW] =  KEYMAP_STACKED
  (___,                         Key_1,           Key_2,          Key_3,           Key_4,       Key_5,       Key_LEDEffectNext,
   Key_Tab,                     Key_Q,           Key_W,          Key_E,           Key_R,       Key_T,       LGUI(Key_I),
   Key_LeftShift,               Key_A,           Key_S,          Key_D,           Key_F,       Key_G,
   Key_LeftControl,               Key_Z,           Key_X,          Key_C,           Key_V,       Key_B,       LGUI(Key_Period),
   Key_LeftGui, Key_Spacebar, Key_Enter, Key_LeftAlt,
   ShiftToLayer(PROGRAMMING_CHARACTERS),

   M(MACRO_ANY),        Key_6,           Key_7,          Key_8,          Key_9,         Key_0,         ___,
   ShiftToLayer(MEDIA), Key_Y,           Key_U,          Key_I,          Key_O,         Key_P,         LGUI(Key_Slash),
                        Key_H,           Key_J,          Key_K,          Key_L,         Key_Semicolon, ___,
   ___,        Key_N,           Key_M,          Key_Comma,      Key_Period,    Key_Slash,     LSHIFT(LGUI(Key_Slash)),
   Key_RightAlt, Key_LeftShift, Key_Backspace, CTL_T(Escape),
   ShiftToLayer(PROGRAMMING_CHARACTERS))
) // KEYMAPS(

/* Re-enable astyle's indent enforcement */
// *INDENT-ON*

/** versionInfoMacro handles the 'firmware version info' macro
 *  When a key bound to the macro is pressed, this macro
 *  prints out the firmware build information as virtual keystrokes
 */

static void versionInfoMacro(uint8_t keyState) {
  if (keyToggledOn(keyState)) {
    Macros.type(PSTR("Keyboardio Model 01 - Kaleidoscope "));
    Macros.type(PSTR(BUILD_INFORMATION));
  }
}

/** anyKeyMacro is used to provide the functionality of the 'Any' key.
 *
 * When the 'any key' macro is toggled on, a random alphanumeric key is
 * selected. While the key is held, the function generates a synthetic
 * keypress event repeating that randomly selected key.
 *
 */

static void anyKeyMacro(uint8_t keyState) {

  static Key lastKey;
  bool toggledOn = false;
  if (keyToggledOn(keyState)) {
    lastKey.setKeyCode(Key_A.getKeyCode() + (uint8_t)(millis() % 36));
    toggledOn = true;
  }

  if (keyIsPressed(keyState))
    Kaleidoscope.hid().keyboard().pressKey(lastKey, toggledOn);
}


/** macroAction dispatches keymap events that are tied to a macro
    to that macro. It takes two uint8_t parameters.

    The first is the macro being called (the entry in the 'enum' earlier in this file).
    The second is the state of the keyswitch. You can use the keyswitch state to figure out
    if the key has just been toggled on, is currently pressed or if it's just been released.

    The 'switch' statement should have a 'case' for each entry of the macro enum.
    Each 'case' statement should call out to a function to handle the macro in question.

 */

const macro_t *macroAction(uint8_t macroIndex, uint8_t keyState) {
  switch (macroIndex) {

  case MACRO_VERSION_INFO:
    versionInfoMacro(keyState);
    break;

  case MACRO_ANY:
    anyKeyMacro(keyState);
    break;
  }
  return MACRO_NONE;
}

// These 'solid' color effect definitions define a rainbow of
// LED color modes calibrated to draw 500mA or less on the
// Keyboardio Model 01.


static kaleidoscope::plugin::LEDSolidColor solidRed(160, 0, 0);
static kaleidoscope::plugin::LEDSolidColor solidOrange(140, 70, 0);
static kaleidoscope::plugin::LEDSolidColor solidYellow(130, 100, 0);
static kaleidoscope::plugin::LEDSolidColor solidGreen(0, 160, 0);
static kaleidoscope::plugin::LEDSolidColor solidBlue(0, 70, 130);
static kaleidoscope::plugin::LEDSolidColor solidIndigo(0, 0, 170);
static kaleidoscope::plugin::LEDSolidColor solidViolet(130, 0, 120);

/** toggleLedsOnSuspendResume toggles the LEDs off when the host goes to sleep,
 * and turns them back on when it wakes up.
 */
void toggleLedsOnSuspendResume(kaleidoscope::plugin::HostPowerManagement::Event event) {
  switch (event) {
  case kaleidoscope::plugin::HostPowerManagement::Suspend:
    LEDControl.set_all_leds_to({0, 0, 0});
    LEDControl.syncLeds();
     break;
  case kaleidoscope::plugin::HostPowerManagement::Resume:
     LEDControl.refreshAll();
    break;
  case kaleidoscope::plugin::HostPowerManagement::Sleep:
    break;
  }
}

/** hostPowerManagementEventHandler dispatches power management events (suspend,
 * resume, and sleep) to other functions that perform action based on these
 * events.
 */
void hostPowerManagementEventHandler(kaleidoscope::plugin::HostPowerManagement::Event event) {
  toggleLedsOnSuspendResume(event);
}

/** This 'enum' is a list of all the magic combos used by the Model 01's
 * firmware The names aren't particularly important. What is important is that
 * each is unique.
 *
 * These are the names of your magic combos. They will be used by the
 * `USE_MAGIC_COMBOS` call below.
 */
enum {
  // Toggle between Boot (6-key rollover; for BIOSes and early boot) and NKRO
  // mode.
  COMBO_TOGGLE_NKRO_MODE
};

/** A tiny wrapper, to be used by MagicCombo.
 * This simply toggles the keyboard protocol via USBQuirks, and wraps it within
 * a function with an unused argument, to match what MagicCombo expects.
 */
static void toggleKeyboardProtocol(uint8_t combo_index) {
  USBQuirks.toggleKeyboardProtocol();
}

/** Magic combo list, a list of key combo and action pairs the firmware should
 * recognise.
 */
USE_MAGIC_COMBOS({.action = toggleKeyboardProtocol,
                  // Left Fn + Esc + Shift
                  .keys = { R3C6, R2C6, R3C7 }
                 });


void tapDanceAction(uint8_t tap_dance_index, byte row, byte col, uint8_t tap_count, kaleidoscope::plugin::TapDance::ActionType tap_dance_action) {
  switch (tap_dance_index) {
  case 0:
    return tapDanceActionKeys(tap_count, tap_dance_action, LGUI(Key_C), LGUI(Key_V));
  case 1:
    return tapDanceActionKeys(tap_count, tap_dance_action, LSHIFT(Key_0), Key_RightBracket);
  case 2:
    return tapDanceActionKeys(tap_count, tap_dance_action, Consumer_ScanNextTrack, Consumer_ScanPreviousTrack);
  case 3:
    return tapDanceActionKeys(tap_count, tap_dance_action, LCTRL(Key_Spacebar), Consumer_PlaySlashPause);
  case 4:
    return tapDanceActionKeys(tap_count, tap_dance_action, LCTRL(LGUI(Key_T)), LCTRL(LALT(LSHIFT(LGUI(Key_T))))
);
  }
}

//LayerHighlighter wowLayerHighlight(WOW);


// First, tell Kaleidoscope which plugins you want to use.
// The order can be important. For example, LED effects are
// added in the order they're listed here.
KALEIDOSCOPE_INIT_PLUGINS(

  // This Qukeys plugin allows you to overload keys on your keyboard so that they
  // produce one keycode (i.e. symbol) when tapped, and a different keycode -- most
  // likely a modifier (e.g. shift or alt) -- when held.
  Qukeys,
  
  // The EEPROMSettings & EEPROMKeymap plugins make it possible to have an
  // editable keymap in EEPROM.
  // EEPROMSettings,
  // EEPROMKeymap,

  // Focus allows bi-directional communication with the host, and is the
  // interface through which the keymap in EEPROM can be edited.
  // Focus,

  // FocusSettingsCommand adds a few Focus commands, intended to aid in
  // changing some settings of the keyboard, such as the default layer (via the
  // `settings.defaultLayer` command)
  // FocusSettingsCommand,

  // FocusEEPROMCommand adds a set of Focus commands, which are very helpful in
  // both debugging, and in backing up one's EEPROM contents.
  // FocusEEPROMCommand,

  // The boot greeting effect pulses the LED button for 10 seconds after the
  // keyboard is first connected
  BootGreetingEffect,

  // The hardware test mode, which can be invoked by tapping Prog, LED and the
  // left Fn button at the same time.
  TestMode,

  // LEDControl provides support for other LED modes
  LEDControl,

  // We start with the LED effect that turns off all the LEDs.
  LEDOff,

  // The rainbow effect changes the color of all of the keyboard's keys at the same time
  // running through all the colors of the rainbow.
  // LEDRainbowEffect,

  // The rainbow wave effect lights up your keyboard with all the colors of a rainbow
  // and slowly moves the rainbow across your keyboard
  LEDRainbowWaveEffect,

  // The chase effect follows the adventure of a blue pixel which chases a red pixel across
  // your keyboard. Spoiler: the blue pixel never catches the red pixel
  // LEDChaseEffect,

  // These static effects turn your keyboard's LEDs a variety of colors
  // solidRed, solidOrange, solidYellow, solidGreen, solidBlue, solidIndigo, solidViolet,

  // The breathe effect slowly pulses all of the LEDs on your keyboard
  LEDBreatheEffect,

  //wowLayerHighlighter,

  //LEDDigitarRainEffect,

  // The stalker effect lights up the keys you've pressed recently
  StalkerEffect,

  // The numpad plugin is responsible for lighting up the 'numpad' mode
  // with a custom LED effect
  NumPad,

  // The macros plugin adds support for macros
  Macros,

  // The MouseKeys plugin lets you add keys to your keymap which move the mouse.
  // MouseKeys,

  // The HostPowerManagement plugin allows us to turn LEDs off when then host
  // goes to sleep, and resume them when it wakes up.
  HostPowerManagement,

  // The MagicCombo plugin lets you use key combinations to trigger custom
  // actions - a bit like Macros, but triggered by pressing multiple keys at the
  // same time.
  MagicCombo,

  // The USBQuirks plugin lets you do some things with USB that we aren't
  // comfortable - or able - to do automatically, but can be useful
  // nevertheless. Such as toggling the key report protocol between Boot (used
  // by BIOSes) and Report (NKRO).
  USBQuirks,

  // Tap-dance keys are general purpose, multi-use keys, which trigger a different action 
  // based on the number of times they were tapped in sequence.
  TapDance
);

/** The 'setup' function is one of the two standard Arduino sketch functions.
 * It's called when your keyboard first powers up. This is where you set up
 * Kaleidoscope and any plugins.
 */

 
void setup() {
  //Qukeys.setHoldTimeout(180);
  //Qukeys.releaseDelayed(20);

  
  // First, call Kaleidoscope's internal setup function
  Kaleidoscope.setup();

  // While we hope to improve this in the future, the NumPad plugin
  // needs to be explicitly told which keymap layer is your numpad layer
  NumPad.numPadLayer = WOW;
  NumPad.lock_hue = 0;

  //wowLayerHighlighter.lockHue = 100; // green(ish)

  // We configure the AlphaSquare effect to use RED letters
  //AlphaSquare.color = CRGB(255, 0, 0);

  // We set the brightness of the rainbow effects to 150 (on a scale of 0-255)
  // This draws more than 500mA, but looks much nicer than a dimmer effect
  LEDRainbowEffect.brightness(150);
  LEDRainbowWaveEffect.brightness(150);

  // Edit the boot greeting so that the butterfly key lights up and not the LED
  // key. Also edit the colour to indigo.
  BootGreetingEffect.hue = 85;

  // The LED Stalker mode has a few effects. The one we like is called
  // 'BlazingTrail'. For details on other options, see
  // https://github.com/keyboardio/Kaleidoscope/blob/master/doc/plugin/LED-Stalker.md
  StalkerEffect.variant = STALKER(Haunt, (CRGB(145, 43, 255)));
  // StalkerEffect.variant = STALKER(BlazingTrail);


  // We want to make sure that the firmware starts with LED effects off
  // This avoids over-taxing devices that don't have a lot of power to share
  // with USB devices
  LEDOff.activate();

  // To make the keymap editable without flashing new firmware, we storehello this thing is prettp dopeuao.euo.uipeoeu.ieuaua.oo
  // additional layers in EEPROM. For now, we reserve space for five layers. If
  // one wants to use these layers, just set the default layer to one in EEPROM,
  // by using the `settings.defaultLayer` Focus command, or by using the
  // `keymap.onlyCustom` command to use EEPROM layers only.
  //EEPROMKeymap.setup(5);
}

/** loop is the second of the standard Arduino sketch functions.
  * As you might expect, it runs in a loop, never exiting.
  *
  * For Kaleidoscope-based keyboard firmware, you usually just want to
  * call Kaleidoscope.loop(); and not do anything custom here.
  */

void loop() {
  Kaleidoscope.loop();
}

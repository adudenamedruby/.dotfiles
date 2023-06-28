function layout() {
    return {
        name: "Shifting Two Pane",
        initialState: {
            mainPaneCount: 1
            // ids: []
        },
        // commands: {
        //     command3: {
        //         description: "Add window to subset",
        //         updateState: (state, focusedWindowID) => {
        //             const ids = state.ids;
        //             if (!!focusedWindowID) {
        //                 const index = ids.indexOf(focusedWindowID);
        //                 if (index === -1) {
        //                     ids.push(focusedWindowID);
        //                 }
        //             }
        //             return { ...state, ids };
        //         }
        //     },
        //     command4: {
        //         description: "Remove window from subset",
        //         updateState: (state, focusedWindowID) => {
        //             const ids = state.ids;
        //             if (!!focusedWindowID) {
        //                 const index = ids.indexOf(focusedWindowID);
        //                 if (index > -1) {
        //                     ids.splice(index, 1);
        //                 }
        //             }
        //             return { ...state, ids };
        //         }
        //     }
        // },
        getFrameAssignments: (windows, screenFrame, state) => {
            const mainPaneCount = Math.min(state.mainPaneCount, windows.length);
            const secondaryPaneCount = windows.length - mainPaneCount;
            const hasSecondaryPane = secondaryPaneCount > 0;

            const secondaryPaneWindowHeight = Math.round(hasSecondaryPane ? (screenFrame.height / secondaryPaneCount) : 0);

            return windows.reduce((frames, window, index) => {
                // const isMain = index < mainPaneCount;
                let frame;
                if (window.isFocused) {
                    frame = {
                        x: screenFrame.x,
                        y: screenFrame.y,
                        width: (screenFrame.width / 3) * 2,
                        height: screenFrame.height
                    };
                } else {
                    frame = {
                        x: screenFrame.x + (screenFrame.width / 3),
                        y: screenFrame.y,
                        width: screenFrame.width / 3,
                        height: screenFrame.height
                    }
                }
                return { ...frames, [window.id]: frame };
            }, {});
        },
        // updateWithChange: (change, state) => {
        //     switch (change.change) {
        //         case "window_swap":
        //             if (state.ids.includes(change.windowID) && !state.ids.includes(change.otherWindowID)) {
        //                 const index = state.ids.indexOf(change.windowID);
        //                 state.ids.splice(index, 1);
        //                 state.ids.push(change.otherWindowID);
        //             } else if (state.ids.includes(change.otherWindowID) && !state.ids.includes(change.windowID)) {
        //                 const index = state.ids.indexOf(change.otherWindowID);
        //                 state.ids.splice(index, 1);
        //                 state.ids.push(change.windowID);
        //             }
        //             break;
        //     }
        //     return state;
        // }
    };
}

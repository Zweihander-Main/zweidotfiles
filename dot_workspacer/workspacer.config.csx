#r "C:\Program Files\workspacer\workspacer.Shared.dll"
#r "C:\Program Files\workspacer\plugins\workspacer.Bar\workspacer.Bar.dll"
#r "C:\Program Files\workspacer\plugins\workspacer.ActionMenu\workspacer.ActionMenu.dll"
#r "C:\Program Files\workspacer\plugins\workspacer.FocusIndicator\workspacer.FocusIndicator.dll"


using System;
using System.Runtime.InteropServices;
using workspacer;
using workspacer.Bar;
using workspacer.ActionMenu;
using workspacer.FocusIndicator;
using workspacer.Bar.Widgets;

Action<IConfigContext> doConfig = (context) =>
{
    var monitors = context.MonitorContainer.GetAllMonitors();

    var titleWidget = new TitleWidget();
    titleWidget.MonitorHasFocusColor = Color.Red;

    context.AddBar(new BarPluginConfig()
    {
        BarTitle = "workspacer.Bar",
        BarHeight = 18,
        FontSize = 10,
        DefaultWidgetForeground = Color.White,
        DefaultWidgetBackground = Color.Black,
        Background = Color.Black,
        LeftWidgets = () => new IBarWidget[] { new WorkspaceWidget(),  new ActiveLayoutWidget(), new TextWidget("    "), titleWidget },
        RightWidgets = () => new IBarWidget[] { new TimeWidget(1000, "ddd, M/dd/yyyy | h:mm tt") },
    });
    context.AddFocusIndicator();
    var actionMenu = context.AddActionMenu();

    context.DefaultLayouts = () => new ILayoutEngine[] { new FullLayoutEngine(), new TallLayoutEngine() };

    var sticky = new StickyWorkspaceContainer(context, StickyWorkspaceIndexMode.Local);
    context.WorkspaceContainer = sticky;
    sticky.CreateWorkspaces(monitors[0], "5|main", "5|code", "5|learn I", "5|learn II", "5|media", "5|chat");
    if (monitors.Length > 1)
    {
        sticky.CreateWorkspaces(monitors[1], "6|main", "6|code", "6|learn I", "6|learn II", "6|media", "6|chat");
    }
    if (monitors.Length > 2)
    {
        sticky.CreateWorkspaces(monitors[2], "8|main", "8|code", "8|learn I", "8|learn II", "8|media", "8|chat");
    }
    if (monitors.Length > 3)
    {
        sticky.CreateWorkspaces(monitors[3], "4|main", "4|code", "4|learn I", "4|learn II", "4|media", "4|chat");
    }

    // Ignore Program Filters
    context.WindowRouter.AddFilter((window) => !window.Title.Equals("Wox"));
    context.WindowRouter.AddFilter((window) => !window.Title.Equals("Everything"));
    // context.WindowRouter.AddFilter((window) => !window.Title.Equals("Cmder"));
    context.WindowRouter.AddFilter((window) => !window.Class.Equals("ApplicationFrameWindow"));
    context.WindowRouter.AddFilter((window) => !window.Title.Equals("MasterStartupHotkeys.ahk"));
    context.WindowRouter.AddFilter((window) => !window.Class.Equals("#32770")); // Deletion dialog
    context.WindowRouter.AddFilter((window) => !window.Class.Equals("OperationStatusWindow")); // Copying dialog
    context.WindowRouter.AddFilter((window) => !window.ProcessName.Equals("pinentry")); // Yubikey GPG
    context.WindowRouter.AddFilter((window) => !window.ProcessName.Equals("qemu-system-x86_64")); // Android Emulator
    context.WindowRouter.AddFilter((window) => !window.ProcessName.Equals("notepad")); // Notepad


    // Router Program Filters
    // context.WindowRouter.AddRoute((window) => window.ProcessName.Equals("thunderbird") ? context.WorkspaceContainer["4|media"] : null);
    // context.WindowRouter.AddRoute((window) => window.ProcessName.Equals("hexchat") ? context.WorkspaceContainer["4|chat"] : null);

    // show keybinds
    // override win keybindds
    // Router program filters doesn't work if program already exists

    // Keybindings
    context.Keybinds.UnsubscribeAll();

    // Monitor bindings to store
    var Mon1NumPad = Keys.NumPad8;
    var Mon2NumPad = Keys.NumPad5;
    var Mon3NumPad = Keys.NumPad4;
    var Mon4Numpad = Keys.NumPad6;
    var MainModKey = KeyModifiers.LAlt;
    var SubModKey = KeyModifiers.LWin;



    // context.Keybinds.Subscribe(MainModKey | KeyModifiers.LShift, Keys.F,
    //     () => context.Enabled = !context.Enabled, "toggle enable/disable");

    context.Keybinds.Subscribe(MainModKey, Keys.C,
        () => context.Workspaces.FocusedWorkspace.CloseFocusedWindow(), "close focused window");

    context.Keybinds.Subscribe(MainModKey, Keys.T,
        () => context.Workspaces.FocusedWorkspace.NextLayoutEngine(), "next layout");

    context.Keybinds.Subscribe(MainModKey | KeyModifiers.LShift, Keys.T,
        () => context.Workspaces.FocusedWorkspace.PreviousLayoutEngine(), "previous layout");

    context.Keybinds.Subscribe(MainModKey | KeyModifiers.LControl, Keys.Back,
        () => context.Workspaces.FocusedWorkspace.ResetLayout(), "reset layout");

    context.Keybinds.Subscribe(MainModKey, Keys.Down,
        () => context.Workspaces.FocusedWorkspace.FocusNextWindow(), "focus next window");

    context.Keybinds.Subscribe(MainModKey, Keys.Up,
        () => context.Workspaces.FocusedWorkspace.FocusPreviousWindow(), "focus previous window");

    context.Keybinds.Subscribe(MainModKey, Keys.M,
        () => context.Workspaces.FocusedWorkspace.FocusPrimaryWindow(), "focus primary window");

    context.Keybinds.Subscribe(MainModKey | KeyModifiers.LShift, Keys.Enter,
        () => context.Workspaces.FocusedWorkspace.SwapFocusAndPrimaryWindow(), "swap focus and primary window");

    context.Keybinds.Subscribe(MainModKey | KeyModifiers.LShift, Keys.Down,
        () => context.Workspaces.FocusedWorkspace.SwapFocusAndNextWindow(), "swap focus and next window");

    context.Keybinds.Subscribe(MainModKey | KeyModifiers.LShift, Keys.Up,
        () => context.Workspaces.FocusedWorkspace.SwapFocusAndPreviousWindow(), "swap focus and previous window");

    context.Keybinds.Subscribe(MainModKey | KeyModifiers.LShift, Keys.Left,
        () => context.Workspaces.FocusedWorkspace.ShrinkPrimaryArea(), "shrink primary area");

    context.Keybinds.Subscribe(MainModKey | KeyModifiers.LShift, Keys.Right,
        () => context.Workspaces.FocusedWorkspace.ExpandPrimaryArea(), "expand primary area");

    context.Keybinds.Subscribe(MainModKey | SubModKey, Keys.OemCloseBrackets,
        () => context.Workspaces.FocusedWorkspace.IncrementNumberOfPrimaryWindows(), "increment # primary windows");

    context.Keybinds.Subscribe(MainModKey | SubModKey, Keys.OemOpenBrackets,
        () => context.Workspaces.FocusedWorkspace.DecrementNumberOfPrimaryWindows(), "decrement # primary windows");

    context.Keybinds.Subscribe(MainModKey | KeyModifiers.LShift, Keys.F,
        () => context.Windows.ToggleFocusedWindowTiling(), "toggle tiling for focused window");

    context.Keybinds.Subscribe(MainModKey, Keys.Q, context.Quit, "quit workspacer");

    context.Keybinds.Subscribe(MainModKey, Keys.R, context.Restart, "restart workspacer");

    context.Keybinds.Subscribe(MainModKey, Keys.D1,
        () => context.Workspaces.SwitchToWorkspace(0), "switch to workspace 1");

    context.Keybinds.Subscribe(MainModKey, Keys.D2,
        () => context.Workspaces.SwitchToWorkspace(1), "switch to workspace 2");

    context.Keybinds.Subscribe(MainModKey, Keys.D3,
        () => context.Workspaces.SwitchToWorkspace(2), "switch to workspace 3");

    context.Keybinds.Subscribe(MainModKey, Keys.D4,
        () => context.Workspaces.SwitchToWorkspace(3), "switch to workspace 4");

    context.Keybinds.Subscribe(MainModKey, Keys.D5,
        () => context.Workspaces.SwitchToWorkspace(4), "switch to workspace 5");

    context.Keybinds.Subscribe(MainModKey, Keys.D6,
        () => context.Workspaces.SwitchToWorkspace(5), "switch to workspace 6");

    context.Keybinds.Subscribe(MainModKey, Keys.D7,
        () => context.Workspaces.SwitchToWorkspace(6), "switch to workspace 7");

    context.Keybinds.Subscribe(MainModKey, Keys.D8,
        () => context.Workspaces.SwitchToWorkspace(7), "switch to workspace 8");

    context.Keybinds.Subscribe(MainModKey, Keys.D9,
        () => context.Workspaces.SwitchToWorkspace(8), "switch to workpsace 9");

    // context.Keybinds.Subscribe(MainModKey, Keys.Left,
    //     () => context.Workspaces.SwitchToPreviousWorkspace(), "switch to previous workspace");

    // context.Keybinds.Subscribe(MainModKey, Keys.Right,
    //     () => context.Workspaces.SwitchToNextWorkspace(), "switch to next workspace");

    context.Keybinds.Subscribe(MainModKey, Mon1NumPad,
        () => context.Workspaces.SwitchFocusedMonitor(3), "focus monitor 1");

    context.Keybinds.Subscribe(MainModKey, Mon2NumPad,
        () => context.Workspaces.SwitchFocusedMonitor(0), "focus monitor 2");

    context.Keybinds.Subscribe(MainModKey, Mon3NumPad,
        () => context.Workspaces.SwitchFocusedMonitor(1), "☻focus monitor 3");

    context.Keybinds.Subscribe(MainModKey, Mon4Numpad,
        () => context.Workspaces.SwitchFocusedMonitor(2), "focus monitor 4");

    context.Keybinds.Subscribe(MainModKey | SubModKey, Mon1NumPad,
        () => context.Workspaces.MoveFocusedWindowToMonitor(3), "move focused window to monitor 1");

    context.Keybinds.Subscribe(MainModKey | SubModKey, Mon2NumPad,
        () => context.Workspaces.MoveFocusedWindowToMonitor(0), "move focused window to monitor 2");

    context.Keybinds.Subscribe(MainModKey | SubModKey, Mon3NumPad,
        () => context.Workspaces.MoveFocusedWindowToMonitor(1), "move focused window to monitor 3");

    context.Keybinds.Subscribe(MainModKey | SubModKey, Mon4Numpad,
        () => context.Workspaces.MoveFocusedWindowToMonitor(2), "move focused window to monitor 4");

    context.Keybinds.Subscribe(MainModKey | KeyModifiers.LShift, Keys.D1,
        () => context.Workspaces.MoveFocusedWindowToWorkspace(0), "switch focused window to workspace 1");

    context.Keybinds.Subscribe(MainModKey | KeyModifiers.LShift, Keys.D2,
        () => context.Workspaces.MoveFocusedWindowToWorkspace(1), "switch focused window to workspace 2");

    context.Keybinds.Subscribe(MainModKey | KeyModifiers.LShift, Keys.D3,
        () => context.Workspaces.MoveFocusedWindowToWorkspace(2), "switch focused window to workspace 3");

    context.Keybinds.Subscribe(MainModKey | KeyModifiers.LShift, Keys.D4,
        () => context.Workspaces.MoveFocusedWindowToWorkspace(3), "switch focused window to workspace 4");

    context.Keybinds.Subscribe(MainModKey | KeyModifiers.LShift, Keys.D5,
        () => context.Workspaces.MoveFocusedWindowToWorkspace(4), "switch focused window to workspace 5");

    context.Keybinds.Subscribe(MainModKey | KeyModifiers.LShift, Keys.D6,
        () => context.Workspaces.MoveFocusedWindowToWorkspace(5), "switch focused window to workspace 6");

    context.Keybinds.Subscribe(MainModKey | KeyModifiers.LShift, Keys.D7,
        () => context.Workspaces.MoveFocusedWindowToWorkspace(6), "switch focused window to workspace 7");

    context.Keybinds.Subscribe(MainModKey | KeyModifiers.LShift, Keys.D8,
        () => context.Workspaces.MoveFocusedWindowToWorkspace(7), "switch focused window to workspace 8");

    context.Keybinds.Subscribe(MainModKey | KeyModifiers.LShift, Keys.D9,
        () => context.Workspaces.MoveFocusedWindowToWorkspace(8), "switch focused window to workspace 9");

    // context.Keybinds.Subscribe(MainModKey | KeyModifiers.LShift, Keys.I,
    //     () => context.Windows.DumpWindowDebugOutput(), "dump debug info to console for all windows");

    // context.Keybinds.Subscribe(MainModKey, Keys.I,
    //     () => context.Windows.DumpWindowUnderCursorDebugOutput(), "dump debug info to console for window under cursor");

    // context.Keybinds.Subscribe(MainModKey | MainModKey | KeyModifiers.LShift, Keys.I,
    //     () => context.ToggleConsoleWindow(), "toggle debug console");

    context.Keybinds.Subscribe(MainModKey | KeyModifiers.LShift, Keys.OemQuestion,
        () => context.Keybinds.ShowKeybindDialog(), "open keybind window");

};
return doConfig;

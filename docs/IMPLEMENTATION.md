# Dotfiles Improvement Plan

Prioritized recommendations for reducing bug fatigue, improving DE compatibility, and enabling cleaner server deployments.

## Status Key

- [ ] Not started
- [~] In progress
- [x] Completed

---

## P0 — Critical (Prevents Breakage)

### [x] 1. Guard `XAUTHORITY` behind startx conditional

**File:** `home/private_dot_config/shell/env.tmpl`

**Problem:** `export XAUTHORITY="${XDG_RUNTIME_DIR}/Xauthority"` was set unconditionally. This breaks any display manager (GDM, SDDM, LightDM) because DMs manage their own Xauthority path. Only `startx` benefits from this override.

**Fix:** Wrapped in a runtime check — only set `XAUTHORITY` when `$DISPLAY` and `$WAYLAND_DISPLAY` are both unset (i.e., in a raw TTY before `startx`). Also wrapped all X11 env vars in a `{{ if ne .chezmoi.os "windows" }}` guard.

### [ ] 2. Add `headless`/`gui` flag to chezmoi template data

**File:** `home/.chezmoi.json.tmpl`

**Problem:** There's no way for chezmoi templates to distinguish between a desktop and a server/CLI-only environment. All GUI env vars, aliases, and configs are deployed everywhere.

**Proposed fix:** Add a `gui` boolean to the chezmoi data template:
```
{{- $gui := promptBool "true/false for GUI/desktop environment" }}
```
Then guard GUI-specific sections in `env.tmpl`, `aliases.tmpl`, and `.chezmoiignore` behind `{{ if .gui }}`.

**Variables to guard:**
- `TERMINAL`, `BROWSER`, `VISUAL` (emacsclient)
- `QT_QPA_PLATFORMTHEME`, `GTK_THEME`, `GTK2_RC_FILES`
- `_JAVA_AWT_WM_NONREPARENTING`
- `ELECTRON_TRASH`
- `ROC_ENABLE_PRE_VEGA`
- `SUDO_ASKPASS`
- X11 variables block (already partially done)

**Aliases to guard:**
- `start`/`open`/`ropen` (xdg-open)
- `pbcopy`/`pbpaste` (xclip)
- `redshifttoggle`/`redshiftinfo`
- `abell`/`nbell` (dunstify, canberra)
- `emacsbreak`

---

## P1 — High (Reduces Recurring Issues)

### [ ] 3. Make activation scripts network-resilient

**Files:**
- `nix/home/zwei/emacs/default.nix`
- `nix/home/zwei/chezmoi/default.nix`
- `nix/home/zwei/shellscripts/default.nix`

**Problem:** Three activation scripts run `git pull` and/or `doom sync` on every `home-manager switch`. These:
- Fail when offline, blocking the entire HM switch
- Are slow (especially `doom sync`)
- Have no idempotency guards (partial failures leave broken state)
- The chezmoi activation's `|| true` silently swallows real errors

**Proposed fix:**
- Add network availability checks before git operations
- Make `doom sync` opt-in (run manually or via a separate script, not on every switch)
- Add proper error handling: distinguish "no network" (skip gracefully) from "real failure" (report)
- For chezmoi: check for uncommitted changes before pulling
- Consider replacing activation scripts with systemd oneshot services that can be retried independently

### [ ] 4. Merge `work.nix` and `play.nix` into shared `desktop.nix`

**Files:**
- `nix/home/zwei/work.nix`
- `nix/home/zwei/play.nix`
- `nix/home/zwei/nixos.nix`

**Problem:** `work.nix` and `play.nix` are nearly identical (existing TODO comment acknowledges this). Differences are only a few modules.

**Proposed fix:** Create a `desktop.nix` that imports all shared desktop modules, then use `hostAttr` flags to conditionally enable the differing modules:
```nix
# desktop.nix — shared desktop config
imports = [
  ./default.nix
  ./alacritty ./coreutils ./copyq ./dunst ./emacs
  ./flameshot ./mouseless ./pcmanfm ./redshift
  ./sound ./stalonetray ./sxhkd ./tmux ./time
  ./udiskie ./vim
];

# Conditionally included based on hostAttr:
# - pipe-viewer (play only currently)
# - shellscripts (work only currently)
# - syncthing, chromium, lf, dev-c, dev-python, anki, x11 (nixos only)
```

Add `hostAttr` options for each optional module group (e.g., `hostAttr.modules.devTools`, `hostAttr.modules.mediaTools`).

### [ ] 5. Guard GUI env vars behind display check

**File:** `home/private_dot_config/shell/env.tmpl`

**Problem:** Variables like `QT_QPA_PLATFORMTHEME`, `GTK_THEME`, `TERMINAL`, `BROWSER` are set on servers, causing warnings and unexpected behavior.

**Proposed fix:** Depends on item #2. Once `gui` flag exists, wrap these in `{{ if .gui }}` blocks.

---

## P2 — Medium (Quality of Life)

### [X] 6. Remove `thefuck`

**File:** `home/private_dot_config/shell/shellrc.tmpl:168-174`

**Problem:** Already marked `# TODO: remove`. `eval "$(thefuck --alias)"` is one of the slowest shell init operations (~0.5-1s). It loads on every interactive shell.

**Fix:** Delete the block.

### [ ] 7. Fix EDITOR to use `emacsclient -a ""` pattern

**File:** `home/private_dot_config/shell/env.tmpl:8`

**Problem:** Current logic runs `pgrep -x emacs` at login time to decide between `emacs` and `emacsclient -nc`. This:
- Races with the systemd emacs service (daemon may not be up yet)
- Result is baked in at login, never re-evaluated
- `pgrep` may not exist on minimal servers

**Proposed fix:**
```bash
export EDITOR="emacsclient -a vim -nc"
```
Or for server awareness (with `gui` flag):
```
{{ if .gui -}}
export EDITOR="emacsclient -a vim -nc"
{{ else -}}
export EDITOR="vim"
{{ end -}}
```
The `-a vim` flag tells emacsclient to fall back to vim if no daemon is running, eliminating the race entirely.

### [ ] 8. Consider migrating server shell config to nix

**Problem:** Server deployments require both nix HM and chezmoi (with git-crypt and GPG) just to get a working shell. This is a high barrier for a new server.

**Proposed fix:** For the server profile, have nix generate a minimal shell config directly:
```nix
# In server.nix or a new shell module
programs.zsh = {
  enable = true;
  dotDir = ".config/zsh";
  history = { ... };
  initExtra = ''
    # Minimal vi bindings, prompt, etc.
  '';
};
```
This lets `home-manager switch` produce a fully working server shell without chezmoi. Desktop environments can continue using chezmoi for the richer config.

### [X] 9. Update `youtube-dl` aliases to `yt-dlp`

**File:** `home/private_dot_config/shell/aliases.tmpl:6-7`

**Problem:** `youtube-dl` is effectively abandoned. `yt-dlp` is the maintained fork with the same CLI interface.

**Fix:**
```bash
alias yt='yt-dlp'
alias ytmp4="yt-dlp -i -f 'bv*[ext=mp4]+ba[ext=m4a]/b[ext=mp4] / bv*+ba/b'"
```

---

## P3 — Low (Cleanup & Maintenance)

### [ ] 10. DRY up zinit null-plugin conditionals

**File:** `home/private_dot_config/shell/shellrc.tmpl:140-181`

**Problem:** Every conditional tool init repeats the same boilerplate pattern:
```bash
if [[ $(command -v 'tool') ]]; then
    zinit wait'0c' lucid light-mode for \
        as"program" id-as'load-script-tool' \
        atload'eval "$(tool init zsh)"' \
        "https://github.com/zdharma-continuum/null"
fi
```

**Proposed fix:** Create a helper function:
```bash
_zinit_load_if_present() {
    local cmd="$1" id="$2" init="$3"
    if command -v "$cmd" &>/dev/null; then
        zinit wait'0c' lucid light-mode for \
            as"program" id-as"$id" \
            atload"$init" \
            "https://github.com/zdharma-continuum/null"
    fi
}
_zinit_load_if_present navi load-scripts-navi 'eval "$(navi widget zsh)"'
_zinit_load_if_present mcfly load-script-mcfly 'eval "$(mcfly init zsh)"'
# etc.
```

### [ ] 11. Audit chezmoi vs nix config overlap

**Problem:** Several tools have configuration split across both chezmoi and nix, which can cause drift:
- **CopyQ:** chezmoi symlink + nix module
- **Systemd user services:** some in `home/private_dot_config/systemd/`, some in nix
- **Git:** chezmoi `private_dot_config/git/` + nix potentially touching git config
- **Shell:** env/aliases in chezmoi, `sessionPath`/`sessionVariables` in nix

**Proposed fix:** For each tool, decide on a single owner:
- **Desktop tools** → nix (since it manages the packages too)
- **Cross-platform configs** (vim, git, shell) → chezmoi (for Windows/WSL compat)
- **Server-only** → nix (to avoid chezmoi dependency)

Document the ownership in a comment at the top of each config.

### [ ] 12. `_JAVA_AWT_WM_NONREPARENTING` is dwm-specific

**File:** `home/private_dot_config/shell/env.tmpl:139`

**Problem:** This env var fixes Java GUI apps in non-reparenting WMs (dwm, bspwm). Under reparenting WMs (KDE, GNOME, i3) it can cause subtle rendering issues.

**Proposed fix:** Guard behind `gui` flag + WM detection, or move it to the dwm startup script (`start_dwm.sh`).

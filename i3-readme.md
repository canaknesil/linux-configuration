# i3 Configuration Instructions

i3 configuration includes the following files:
- `i3.conf`: The bulk of the configuration is in this file. It should remain in the configuration repository.
- `i3-main.conf`: The content of this file is to be copied to `~/.config/i3/config` or `~/.config/sway/config`, and necessary variables to be set.
- `i3-only.conf` and `sway-only.conf`: Configuration specific to i3 and Sway, respectively. They should remain in the configuration repository.
- `i3-status.conf`: To be copied to `~/.config/i3status/config`.
- `waybar-config.jsonc`: To be copied to `~/.config/waybar/config.jsonc`.
- `waybar-style.css`: To be copied to `~/.config/waybar/style.css`.

## Multiple monitors with i3

Apply and save a layout configuration using arandr GUI, by default to
directory "~/.screenlayout". A saved layout configuration can be
applied manually by running the saved scripts. The applied
configurations aren't permanent and don't react to hutplugs.

Use autorandr to save a display profile (mapping of a set of monitor
connections to a layout configuration), so that the configuration is
applied automatically when a particular set of monitor connections is
detected.

- `autorandr`: see saved profiles and the one that is detected and active
- `autorandr --save office`: create a profile named "office" from the current connections and configuration

## Multiple keyboard layouts with i3

Using IBus preferences GUI, add keyboard methods and define a shortcut
`<Alt><Super>space` for selecting the next input method.

## Default browser

List all desktop files:
```
find /usr/share/applications ~/.local/share/applications /var/lib/snapd/desktop/applications /var/lib/flatpak/exports/share/applications -name '*.desktop' 2>/dev/null | grep firefox
```

```
xdg-settings get default-web-browser
xdg-settings set default-web-browser firefox.desktop
```

The following may also be necessary:
```
xdg-mime query default x-scheme-handler/http
xdg-mime default firefox.desktop x-scheme-handler/http
xdg-mime default firefox.desktop x-scheme-handler/https
xdg-mime default firefox.desktop text/html
```

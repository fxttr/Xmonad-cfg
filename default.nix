{ pkgs, lib, ... }:

let
  extra = ''
    set +x
    ${pkgs.util-linux}/bin/setterm -blank 0 -powersave off -powerdown 0
    ${pkgs.xorg.xset}/bin/xset s off
    ${pkgs.xcape}/bin/xcape -e "Hyper_L=Tab;Hyper_R=backslash"
    ${pkgs.xorg.setxkbmap}/bin/setxkbmap -option ctrl:nocaps
  '';

  hdmiExtra = ''
    ${pkgs.xorg.xrandr}/bin/xrandr xrandr --output HDMI-0 --primary --mode 1920 x1080 --pos 0 x0 --rotate normal --output DP-0 --off --output DP-1 --mode 1920 x1080 --pos 1920 x0 --rotate normal --output DP-2 --off --output DP-3 --off --output DP-4 --off --output DP-5 --off
  '';
in
{
  xresources.properties = {
    "Xft.autohint" = 0;
    "Xft.hintstyle" = "hintfull";
    "Xft.hinting" = 1;
    "Xft.antialias" = 1;
    "Xft.rgba" = "rgb";
    "Xcursor*theme" = "Vanilla-DMZ-AA";
  };

  home.packages = [
    pkgs.xmobar
    pkgs.feh
    pkgs.rxvt-unicode
    pkgs.ranger
    pkgs.dmenu
  ];

  xsession = {
    enable = true;

    initExtra = extra + hdmiExtra;

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = hp: [
        hp.dbus
        hp.monad-logger
      ];
      config = ./config.hs;
    };
  };
}

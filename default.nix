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
    ${pkgs.xorg.xrandr}/bin/xrandr --output DVI-D-0 --rotate left
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

  programs.xmobar = {
    enable = true;
    extraConfig = ''
      Config { font = "xft:Source Code Pro:size=9:regular:antialias=true"
          , overrideRedirect = False
          , borderColor = "#282A36"
          , border = TopB
          , bgColor = "#282A36"
          , fgColor = "#f8f8f2"
          , position = TopW L 100 
          , commands = [    Run Cpu ["-L","3","-H","50","--normal","#98c379","--high","#e06c75"] 10
                          , Run Memory ["-t","Mem: <usedratio>%"] 10
                          , Run Com "uname" ["-s","-r"] "" 36000
                          , Run Date "%a %b %_d %Y %H:%M:%S" "date" 10
                          , Run StdinReader
                          ]
          , sepChar = "%" 
          , alignSep = "}{"
          , template = "%StdinReader% }{ %cpu% | %memory% | <fc=#ff79c6>%date%</fc> | %uname%"
          }
    '';
  };

  xsession = {
    enable = true;

    initExtra = extra;

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

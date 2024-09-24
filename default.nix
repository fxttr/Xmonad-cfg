{ pkgs, lib, inputs, ... }:

let
  colorscheme = import ./config/colors.nix;
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
  services.dunst = {
    enable = true;
    settings = {
      global = {
        monitor = 0;
        follow = "mouse";
        geometry = "300x60-15+46";
        indicate_hidden = "yes";
        shrink = "yes";
        transparency = 0;
        notification_height = 0;
        separator_height = 2;
        padding = 8;
        horizontal_padding = 8;
        frame_width = 3;
        frame_color = "#000000";
        separator_color = "frame";
        sort = "yes";
        idle_threshold = 120;
        font = "Source Code Pro1 12";
        line_height = 0;
        markup = "full";
        format = "<b>%s</b>\n%b";
        alignment = "left";
        show_age_threshold = 60;
        word_wrap = "yes";
        ellipsize = "middle";
        ignore_newline = "no";
        stack_duplicates = true;
        hide_duplicate_count = false;
        show_indicators = "yes";
        icon_position = "left";
        max_icon_size = 32;
        sticky_history = "yes";
        history_length = 20;
        title = "Dunst";
        class = "Dunst";
        startup_notification = false;
        verbosity = "mesg";
        corner_radius = 8;
        mouse_left_click = "close_current";
        mouse_middle_click = "do_action";
        mouse_right_click = "close_all";
      };

      urgency_low = {
        foreground = "#ffd5cd";
        background = "#121212";
        frame_color = "#181A20";
        timeout = 10;
      };

      urgency_normal = {
        background = "#121212";
        foreground = "#ffd5cd";
        frame_color = "#181A20";
        timeout = 10;
      };

      urgency_critical = {
        background = "#121212";
        foreground = "#ffd5cd";
        frame_color = "#181A20";
        timeout = 0;
      };
    };
  };

  programs.alacritty = {
    enable = true;
    settings = {
      colors.primary = {
        background = "#${colorscheme.dark.bg_0}";
        foreground = "#${colorscheme.dark.fg_0}";
        dim_foreground = "#${colorscheme.dark.dim_0}";
      };

      colors.normal = {
        black = "#636363";
        red = "#${colorscheme.dark.red}";
        green = "#${colorscheme.dark.green}";
        yellow = "#${colorscheme.dark.yellow}";
        blue = "#${colorscheme.dark.blue}";
        magenta = "#${colorscheme.dark.magenta}";
        cyan = "#${colorscheme.dark.cyan}";
        white = "#f7f7f7";
      };

      colors.bright = {
        black = "#636363";
        red = "#${colorscheme.dark.br_red}";
        green = "#${colorscheme.dark.br_green}";
        yellow = "#${colorscheme.dark.br_yellow}";
        blue = "#${colorscheme.dark.br_blue}";
        magenta = "#${colorscheme.dark.br_magenta}";
        cyan = "#${colorscheme.dark.br_cyan}";
        white = "#f7f7f7";
      };
    };
  };

  services.polybar = {
    enable = true;
    config = ./config/polybar.ini;
    script = "polybar mainBar &";
  };

  home.packages = [
    pkgs.feh
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
      config = ./app/Xmonad.hs;
    };
  };
}

-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")
-- Widget and layout library
local wibox = require("wibox")
local vicious = require("vicious")
-- Theme handling library
local beautiful = require("beautiful")
-- Notification library
local naughty = require("naughty")
local menubar = require("menubar")
local hotkeys_popup = require("awful.hotkeys_popup").widget

--local freedesktop = require("freedesktop")

-- Load Debian menu entries
require("debian.menu")

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
    naughty.notify({ preset = naughty.config.presets.critical,
                     title = "Oops, there were errors during startup!",
                     text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
    local in_error = false
    awesome.connect_signal("debug::error", function (err)
        -- Make sure we don't go into an endless error loop
        if in_error then return end
        in_error = true

        naughty.notify({ preset = naughty.config.presets.critical,
                         title = "Oops, an error happened!",
                         text = tostring(err) })
        in_error = false
    end)
end
-- }}}


--- Spawns cmd if no client can be found matching properties
-- If such a client can be found, pop to first tag where it is visible, and give it focus
-- @param cmd the command to execute
-- @param properties a table of properties to match against clients.  Possible entries: any properties of the client object
function run_or_raise(cmd, properties)
   local clients = client.get()
   local focused = awful.client.next(0)
   local findex = 0
   local matched_clients = {}
   local n = 0
   for i, c in pairs(clients) do
      --make an array of matched clients
      if match(properties, c) then
         n = n + 1
         matched_clients[n] = c
         if c == focused then
            findex = n
         end
      end
   end
   if n > 0 then
      local c = matched_clients[1]
      -- if the focused window matched switch focus to next in list
      if 0 < findex and findex < n then
         c = matched_clients[findex+1]
      end
      local ctags = c:tags()
      if #ctags == 0 then
         -- ctags is empty, show client on current tag
         local curtag = awful.tag.selected()
         awful.client.movetotag(curtag, c)
      else
         -- Otherwise, pop to first tag client is visible on
         awful.tag.viewonly(ctags[1])
      end
      -- And then focus the client
      client.focus = c
      c:raise()
      return
   end
   awful.spawn(cmd)
end

-- Returns true if all pairs in table1 are present in table2
function match (table1, table2)
   for k, v in pairs(table1) do
      if table2[k] ~= v and not table2[k]:find(v) then
         return false
      end
   end
   return true
end


-- {{{ Variable definitions

homedir = os.getenv("HOME")

-- Themes define colours, icons, font and wallpapers.
--beautiful.init(awful.util.get_themes_dir() .. "default/theme.lua")
beautiful.init(homedir .. "/.config/awesome/themes/xresources/theme.lua")

-- This is used later as the default terminal and editor to run.
terminal = "x-terminal-emulator"
editor = os.getenv("EDITOR") or "editor"
editor_cmd = terminal .. " -e " .. editor

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
awful.layout.layouts = {
    awful.layout.suit.tile,
    awful.layout.suit.max,
    awful.layout.suit.max.fullscreen,
    awful.layout.suit.floating,
}
-- }}}

-- {{{ MY WIDGETS

-- newmail

newmailwidget = wibox.widget.textbox()
newmailwidget.text = "N"
newmailwidgettimer = timer({ timeout = 30 })
newmailwidgettimer:connect_signal("timeout",
  function()
    status = io.popen("find "..homedir.."/.mail/minik/INBOX/new -type f | wc -l", "r")
    newmail = status:read()
    if newmail == nil then
        newmailwidget.markup = '<span color="#535d6c">N</span>'                     
    else
       newmail = tonumber(newmail)
       if newmail > 0 then
	  newmailwidget.markup = '<span color="red">N</span>'
       else
	  newmailwidget.markup = '<span color="#535d6c">N</span>'
       end
    end
    status:close()    
  end    
)    
newmailwidgettimer:start()

-- metar widget

metarwidget = wibox.widget 
{
    text = "WAITING FOR METAR DATA",
    widget = wibox.widget.textbox	
}

-- VPN widget

vpnwidget = wibox.widget.textbox()
vpnwidget.text = " VPN "
vpnwidgettimer = timer({ timeout = 10 })
vpnwidgettimer:connect_signal("timeout",
  function()
    status = io.popen("ip addr | grep tun0 2>/dev/null", "r")
    if status:read() == nil then
        vpnwidget.markup = ' <span color="#FF0000">VPN</span> <span color="#000">|</span>'                     
    else
        vpnwidget.markup = ' <span color="#00FF00">VPN</span> <span color="#000">|</span>'
    end
    status:close()    
  end    
)    
vpnwidgettimer:start()

--- }}}

--- {{{ OTHER WIDGETS

bat_widget = wibox.widget.textbox()
vicious.register(bat_widget, vicious.widgets.bat, " <span color='white'>$1$2%</span> ", 32, "BAT0")

cpuwidget = wibox.widget.textbox()
cpuwidget:set_forced_width(170)
--cpuwidget:set_source(color(beautiful.fg_normal))
vicious.register(cpuwidget, vicious.widgets.cpu, " <span color='#535d6c'>CPU $1 $2 $3 $4</span>")

--- }}}

-- {{{ Helper functions
local function client_menu_toggle_fn()
    local instance = nil

    return function ()
        if instance and instance.wibox.visible then
            instance:hide()
            instance = nil
        else
            instance = awful.menu.clients({ theme = { width = 250 } })
        end
    end
end
-- }}}

-- {{{ Menu
-- Create a launcher widget and a main menu
myawesomemenu = {
   { "hotkeys", function() return false, hotkeys_popup.show_help end},
   { "manual", terminal .. " -e man awesome" },
   { "edit config", editor_cmd .. " " .. awesome.conffile },
   { "restart", awesome.restart },
   { "quit", function() awesome.quit() end}
}

mymainmenu = awful.menu({ items = { { "awesome", myawesomemenu, beautiful.awesome_icon },
                                    { "Debian", debian.menu.Debian_menu.Debian },
    { "Nautilus", "/usr/bin/nautilus" },
    { "Chromium", "/usr/bin/chromium --incognito" },
    { "Tor Browser", function () awful.util.spawn_with_shell(homedir .. "/usr/tor-browser_en-US/Browser/start-tor-browser") end },
    { "Thunderbird", "/usr/bin/thunderbird" },
    { "Libreoffice", "/usr/bin/libreoffice" },
    { "Calendar", function () awful.util.spawn_with_shell("LC_TIME=pl_PL.utf8 /usr/bin/gnome-calendar") end },
    { "Shotwell", "/usr/bin/shotwell" },
    { "Gimp", "/usr/bin/gimp" },
    { "Stellarium", "/usr/bin/stellarium" },
    { "rtorrent", "rxvt -sr -T rtorrent -n rtorrent -e " .. homedir .."/bin/ssh kim.dominowisla.pl -Xt screen -aAdr -RR rtorrent rtorrent" },
    { "open terminal", terminal },
    { "suspend", homedir .. '/bin/my_shutdown.sh Suspend' },
    { "shutdown", homedir .. '/bin/my_shutdown.sh Shutdown' },
    { "reboot", homedir .. '/bin/my_shutdown.sh Reboot' },
    { "lock", homedir .. '/bin/my_shutdown.sh LockScreen' }
                                  }
                        })

-- MY
mysshmenu = awful.menu({ items = { 
    { "ssh adm", "rxvt -e sh -c 'TERM=xterm " .. homedir .."/bin/ssh adm'" },
    { "ssh linode", "rxvt -e sh -c 'TERM=xterm " .. homedir .."/bin/ssh linode'" },
    { "ssh linode2", "rxvt -e sh -c 'TERM=xterm " .. homedir .."/bin/ssh linode2'" },
    { "ssh adm -l www-data", "rxvt -e sh -c 'TERM=xterm " .. homedir .."/bin/ssh adm -l www-data'" },
    { "mail.log linode", "rxvt -name linode_mail.log -title linode_mail.log -e sh -c '"..homedir.."/bin/ssh -t linode less /var/log/mail.log'" },
    { "mail.log linode2", "rxvt -name linode2_mail.log -title linode2_mail.log -e sh -c '"..homedir.."/bin/ssh -t linode2 less /var/log/mail.log'" },
    { "adm php.log", "rxvt -name adm_php.log -title adm_php.log -e sh -c '"..homedir.."/bin/ssh -t www-data@adm less /var/www/adm/log/php5'" },
    { "open terminal", terminal }
  }, width = 300 
})

mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon,
                                     menu = mymainmenu })

-- Menubar configuration
menubar.utils.terminal = terminal -- Set the terminal for applications that require it
-- }}}

-- Keyboard map indicator and switcher
mykeyboardlayout = awful.widget.keyboardlayout()

-- {{{ Wibar
-- Create a textclock widget
mytextclock = wibox.widget.textclock()

-- Create a wibox for each screen and add it
local taglist_buttons = awful.util.table.join(
                    awful.button({ }, 1, function(t) t:view_only() end),
                    awful.button({ modkey }, 1, function(t)
                                              if client.focus then
                                                  client.focus:move_to_tag(t)
                                              end
                                          end),
                    awful.button({ }, 3, awful.tag.viewtoggle),
                    awful.button({ modkey }, 3, function(t)
                                              if client.focus then
                                                  client.focus:toggle_tag(t)
                                              end
                                          end),
                    awful.button({ }, 4, function(t) awful.tag.viewnext(t.screen) end),
                    awful.button({ }, 5, function(t) awful.tag.viewprev(t.screen) end)
                )

local tasklist_buttons = awful.util.table.join(
                     awful.button({ }, 1, function (c)
                                              if c == client.focus then
                                                  c.minimized = true
                                              else
                                                  -- Without this, the following
                                                  -- :isvisible() makes no sense
                                                  c.minimized = false
                                                  if not c:isvisible() and c.first_tag then
                                                      c.first_tag:view_only()
                                                  end
                                                  -- This will also un-minimize
                                                  -- the client, if needed
                                                  client.focus = c
                                                  c:raise()
                                              end
                                          end),
                     awful.button({ }, 3, client_menu_toggle_fn()),
                     awful.button({ }, 4, function ()
                                              awful.client.focus.byidx(1)
                                          end),
                     awful.button({ }, 5, function ()
                                              awful.client.focus.byidx(-1)
                                          end))

local function set_wallpaper(s)
    -- Wallpaper
    if beautiful.wallpaper then
        local wallpaper = beautiful.wallpaper
        -- If wallpaper is a function, call it with the screen
        if type(wallpaper) == "function" then
            wallpaper = wallpaper(s)
        end
        gears.wallpaper.maximized(wallpaper, s, true)
    end
end

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal("property::geometry", set_wallpaper)

awful.screen.connect_for_each_screen(function(s)
    -- Wallpaper
    set_wallpaper(s)

    -- Each screen has its own tag table.
    awful.tag({ "1", "2", "3", "4", "5", "6", "7", "8", "9" }, s, awful.layout.layouts[1])

    -- Create a promptbox for each screen
    s.mypromptbox = awful.widget.prompt()
    -- Create an imagebox widget which will contains an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    s.mylayoutbox = awful.widget.layoutbox(s)
    s.mylayoutbox:buttons(awful.util.table.join(
                           awful.button({ }, 1, function () awful.layout.inc( 1) end),
                           awful.button({ }, 3, function () awful.layout.inc(-1) end),
                           awful.button({ }, 4, function () awful.layout.inc( 1) end),
                           awful.button({ }, 5, function () awful.layout.inc(-1) end)))
    -- Create a taglist widget
    s.mytaglist = awful.widget.taglist(s, awful.widget.taglist.filter.all, taglist_buttons)

    -- Create a tasklist widget
    s.mytasklist = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, tasklist_buttons)

    -- Create the wibox
    s.mywibox = awful.wibar({ position = "top", screen = s })
    s.mywibox2 = awful.wibar({ position = "bottom", screen = s })

    -- Add widgets to the wibox
    s.mywibox:setup {
        layout = wibox.layout.align.horizontal,
        { -- Left widgets
            layout = wibox.layout.fixed.horizontal,
            mylauncher,
            s.mytaglist,
            s.mypromptbox,
        },
        s.mytasklist, -- Middle widget
        
        { -- Right widgets
            layout = wibox.layout.fixed.horizontal,
            mykeyboardlayout,
            wibox.widget.systray(),
	    bat_widget,
	    newmailwidget,
            mytextclock,
            s.mylayoutbox,
        },
    }

    s.mywibox2:setup {
       layout = wibox.layout.align.horizontal,
       {
           layout = wibox.layout.fixed.horizontal,
	   {
	       metarwidget,
	       fg = "#535d6c",
	       widget = wibox.container.background
	   }
       },
       {
           layout = wibox.layout.align.horizontal,
           expand = "outside",
           nil,
       }, 
       {
           layout = wibox.layout.fixed.horizontal,
           vpnwidget,
           cpuwidget,
       }
    }

end)
-- }}}

-- {{{ Mouse bindings
root.buttons(awful.util.table.join(
    awful.button({ }, 3, function () mymainmenu:toggle() end),
    awful.button({ }, 4, awful.tag.viewnext),
    awful.button({ }, 5, awful.tag.viewprev)
))
-- }}}

-- {{{ Key bindings
globalkeys = awful.util.table.join(
    awful.key({ modkey, "Shift"   }, "s",      hotkeys_popup.show_help,
              {description="show help", group="awesome"}),
    awful.key({ modkey,           }, "Left",   awful.tag.viewprev,
              {description = "view previous", group = "tag"}),
    awful.key({ modkey,           }, "Right",  awful.tag.viewnext,
              {description = "view next", group = "tag"}),
    awful.key({ modkey,           }, "Escape", awful.tag.history.restore,
              {description = "go back", group = "tag"}),

    awful.key({ modkey,           }, "j",
        function ()
            awful.client.focus.byidx( 1)
        end,
        {description = "focus next by index", group = "client"}
    ),
    awful.key({ modkey,           }, "k",
        function ()
            awful.client.focus.byidx(-1)
        end,
        {description = "focus previous by index", group = "client"}
    ),
    awful.key({ modkey,           }, "w", function () mymainmenu:show() end,
              {description = "show main menu", group = "awesome"}),
    --MY
    awful.key({ modkey,           }, "s", function () mysshmenu:show() end,
              {description = "show ssh menu", group = "awesome"}),

    -- Layout manipulation
    awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end,
              {description = "swap with next client by index", group = "client"}),
    awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end,
              {description = "swap with previous client by index", group = "client"}),
    awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end,
              {description = "focus the next screen", group = "screen"}),
    -- duplicated for easier key
    awful.key({ modkey,           }, "x", function () awful.screen.focus_relative( 1) end,
              {description = "focus the next screen", group = "screen"}),
    awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end,
              {description = "focus the previous screen", group = "screen"}),
    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto,
              {description = "jump to urgent client", group = "client"}),
    awful.key({ modkey,           }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end,
        {description = "go back", group = "client"}),

    -- Standard program
    awful.key({ modkey,           }, "Return", function () awful.spawn(terminal) end,
              {description = "open a terminal", group = "launcher"}),
    awful.key({ modkey, "Control" }, "r", awesome.restart,
              {description = "reload awesome", group = "awesome"}),
    awful.key({ modkey, "Shift"   }, "q", awesome.quit,
              {description = "quit awesome", group = "awesome"}),

    -- BEGIN MY

    -- Brightness
    awful.key({ 		  }, "XF86MonBrightnessUp",   function () awful.util.spawn("xbacklight -inc 7") end,
    		{description = "brightness up", group = "My custom keys"}),
    awful.key({ 		  }, "XF86MonBrightnessDown", function () awful.util.spawn("xbacklight -dec 7") end,
    		{description = "brightness down", group = "My custom keys"}),
    awful.key({ modkey,		  }, "F11",   function () awful.util.spawn(homedir .. "/bin/redshift_launcher") end,
    		{description = "toggle redshift", group = "My custom keys"}),
    awful.key({ modkey, "Control" }, "t", function () awful.util.spawn(homedir .. "/bin/my_tap") end,
    		{description = "toggle touchpad", group = "My custom keys"}),

    -- Volume
    awful.key({ 		  }, "XF86AudioRaiseVolume",  function () awful.util.spawn_with_shell("~/bin/pa_vol_up") end,
    		{description = "volume up", group = "My custom keys"}),
    awful.key({ 		  }, "XF86AudioLowerVolume",  function () awful.util.spawn_with_shell("~/bin/pa_vol_down") end,
    		{description = "volume down", group = "My custom keys"}),

    -- LockScreen & Suspend
    awful.key({ 		  }, "F12",   function () awful.util.spawn(homedir .. "/bin/my_shutdown.sh LockScreen") end,
    		{description = "lock screen", group = "My custom keys"}),
    awful.key({ modkey,		  }, "F12",   function () awful.util.spawn(homedir .. "/bin/my_shutdown.sh Suspend") end,
    		{description = "suspend", group = "My custom keys"}),

    -- profanity
    awful.key({ modkey,		  }, "p", function () run_or_raise("rxvt -name profanity -title profanity -e sh -c 'LD_LIBRARY_PATH=/usr/local/lib profanity'", { instance = "profanity" } ) end,
    		{description = "profanity", group = "My custom keys"}),
    -- floating term 
    awful.key({ modkey, "Shift"	  }, "Return",  function () run_or_raise("rxvt -name float -title float -tr -sh 50 -geometry 159x12", { instance = "float" } ) end,
    		{description = "open floating terminal", group = "My custom keys"}),
    -- ping 8.8.8.8
    awful.key({ modkey, "Shift"	  }, "p", function () run_or_raise('rxvt -name float -title float -tr -sh 50 -geometry 80x12 -e sh -c "ping 8.8.8.8"', { instance = "float" } ) end,
    		{description = "ping 8.8.8.8", group = "My custom keys"}),
    -- edit rc.lua
    awful.key({ modkey,		  }, ",", function () run_or_raise("rxvt -name rc.lua -title rc.lua -e sh -c 'emacs -nw ~/.config/awesome/rc.lua'", { instance = "rc.lua" } ) end,
    		{description = "open rc.lua settings", group = "My custom keys"}),
    -- mutt
    awful.key({ modkey,		  }, "y", function () run_or_raise("rxvt -name mutt -e 'mutt'", { instance = "mutt" } ) end,
    		{description = "mutt", group = "My custom keys"}),
    -- irssi
    awful.key({ modkey, "Shift"	  }, "o", function () run_or_raise("rxvt -sr -T irssi -n irssi -e "..homedir.."/bin/ssh adm -Xt screen -aAdr -RR irssi irssi", { name = "irssi" }) end,  
    		{description = "irssi", group = "My custom keys"}),
    -- firefox
    awful.key({ modkey, 	  }, "i", function () run_or_raise("firefox-esr -private-window", { name = "Firefox" }) end,  
    		{description = "firefox", group = "My custom keys"}),
    -- chromium
    awful.key({ modkey, "Shift"   }, "i", function () run_or_raise("chromium --incognito", { name = "Chromium" }) end,  
    		{description = "chromium", group = "My custom keys"}),
    -- tor browser bundle
    --awful.key({ modkey, 	  }, "t", function () run_or_raise("torbrowser-launcher", { name = "Tor Browser" }) end,  
    --		{description = "tor browser", group = "My custom keys"}),
 
    -- pavucontrol
    awful.key({ modkey, 	  }, "v", function () run_or_raise("pavucontrol", { name = "Volume Control" }) end,  
    		{description = "pavucontrol", group = "My custom keys"}),
    -- END MY

    awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)          end,
              {description = "increase master width factor", group = "layout"}),
    awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)          end,
              {description = "decrease master width factor", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1, nil, true) end,
              {description = "increase the number of master clients", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1, nil, true) end,
              {description = "decrease the number of master clients", group = "layout"}),
    awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1, nil, true)    end,
              {description = "increase the number of columns", group = "layout"}),
    awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1, nil, true)    end,
              {description = "decrease the number of columns", group = "layout"}),
    awful.key({ modkey,           }, "space", function () awful.layout.inc( 1)                end,
              {description = "select next", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(-1)                end,
              {description = "select previous", group = "layout"}),

    awful.key({ modkey, "Control" }, "n",
              function ()
                  local c = awful.client.restore()
                  -- Focus restored client
                  if c then
                      client.focus = c
                      c:raise()
                  end
              end,
              {description = "restore minimized", group = "client"}),

    -- Prompt
    awful.key({ modkey },            "r",     function () awful.screen.focused().mypromptbox:run() end,
              {description = "run prompt", group = "launcher"}),

--    awful.key({ modkey }, "x",
--              function ()
--                  awful.prompt.run {
--                  prompt       = "Run Lua code: ",
--                textbox      = awful.screen.focused().mypromptbox.widget,
--              exe_callback = awful.util.eval,
--            history_path = awful.util.get_cache_dir() .. "/history_eval"
--        }
--  end,
              -- {description = "lua execute prompt", group = "awesome"}),
    -- Menubar -- MY mod: było p
    awful.key({ modkey, }, "a", function() menubar.show() end,
              {description = "show the menubar", group = "launcher"})
)

clientkeys = awful.util.table.join(
    awful.key({ modkey,           }, "f",
        function (c)
            c.fullscreen = not c.fullscreen
            c:raise()
        end,
        {description = "toggle fullscreen", group = "client"}),
    awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end,
              {description = "close", group = "client"}),
    awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ,
              {description = "toggle floating", group = "client"}),
    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end,
              {description = "move to master", group = "client"}),
    awful.key({ modkey,           }, "o",      function (c) c:move_to_screen()               end,
              {description = "move to screen", group = "client"}),
    awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end,
              {description = "toggle keep on top", group = "client"}),
    awful.key({ modkey,           }, "n",
        function (c)
            -- The client currently has the input focus, so it cannot be
            -- minimized, since minimized clients can't have the focus.
            c.minimized = true
        end ,
        {description = "minimize", group = "client"}),
    awful.key({ modkey,           }, "m",
        function (c)
            c.maximized = not c.maximized
            c:raise()
        end ,
        {description = "maximize", group = "client"})
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
    globalkeys = awful.util.table.join(globalkeys,
        -- View tag only.
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
                        local screen = awful.screen.focused()
                        local tag = screen.tags[i]
                        if tag then
                           tag:view_only()
                        end
                  end,
                  {description = "view tag #"..i, group = "tag"}),
        -- Toggle tag display.
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = awful.screen.focused()
                      local tag = screen.tags[i]
                      if tag then
                         awful.tag.viewtoggle(tag)
                      end
                  end,
                  {description = "toggle tag #" .. i, group = "tag"}),
        -- Move client to tag.
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:move_to_tag(tag)
                          end
                     end
                  end,
                  {description = "move focused client to tag #"..i, group = "tag"}),
        -- Toggle tag on focused client.
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:toggle_tag(tag)
                          end
                      end
                  end,
                  {description = "toggle focused client on tag #" .. i, group = "tag"})
    )
end

clientbuttons = awful.util.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize))

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = awful.client.focus.filter,
                     raise = true,
                     keys = clientkeys,
                     buttons = clientbuttons,
                     screen = awful.screen.preferred,
                     placement = awful.placement.no_overlap+awful.placement.no_offscreen
     }
    },
    

    -- Floating clients.
    { rule_any = {
        instance = {
          "DTA",  -- Firefox addon DownThemAll.
          "copyq",  -- Includes session name in class.
        },
        class = {
          "mpv",
          "Evince",
	  "mplayer2",
	  "Skype",
	  "Vlc",
	  "Viewnior",
	  "Pavucontrol",
	  "gimp",
          "Arandr",
          "Gpick",
          "Kruler",
          "MessageWin",  -- kalarm.
          "Sxiv",
          "Wpa_gui",
          "pinentry-gnome3",
          "Gcr-prompter",
	  "pinentry",
	  "veromix",
          "xtightvncviewer"},

        name = {
          "Event Tester",  -- xev.
        },
        role = {
          "AlarmWindow",  -- Thunderbird's calendar.
          "pop-up",       -- e.g. Google Chrome's (detached) Developer Tools.
        }
      }, properties = { floating = true, placement = awful.placement.centered }},

    -- Add titlebars to normal clients and dialogs
    { rule_any = {type = { "normal", "dialog" }
      }, properties = { titlebars_enabled = false }
    },
    
    { rule_any = { class = { "Skype", "Viewnior" }}, properties = { titlebars_enabled = true } },

    -- Set Firefox to always map on the tag named "2" on screen 1.
    -- { rule = { class = "Firefox" },
    --   properties = { screen = 1, tag = "2" } },
    
    -- BEGIN MY
--    { rule = { class = "Tor Browser" },
--      properties = { screen = 1, tag = "2", switchtotag = true, focus = true, 
---                     width = 1368, height = 841, floating = true, 
--	             border_width=1, border_color='#000000', x=276, y=119 } },
 --   { rule = { class = "Torbrowser-launcher" },
--      properties = { screen = 1, tag = "2", switchtotag = true, focus = true, 
--                     floating = true, border_width=5,
 --                    border_color='#000000', x=470 } },
 --   { rule = { instance = "TorLauncher" },
 --     properties = { screen = 1, tag = "2", switchtotag = true, focus = true, 
 --                    floating = true, border_width=5,
 --                    border_color='#000000', x=470 } },
    { rule = { class = "Firefox" },
      properties = { screen = 1, tag = "1", switchtotag = true, focus = true  } },
    { rule = { class = "Firefox", instance = "Browser" },
      properties = { screen = 1, tag = "1" },
      		     callback = awful.client.setslave },
    { rule = { class = "Firefox", role = "pop-up" },
      properties = { screen = 1, tag = "1" },
      		     callback = awful.client.setslave },
    { rule = { class = "URxvt", instance = "dmesg" },
      properties = { focus = false },
      		     callback = awful.client.setslave },
    { rule = { class = "XTerm", instance = "mutt" },
      properties = { screen = 1, tag = "1", switchtotag = true, focus = true },
      		     callback = awful.client.setslave, 
      		     awful.client.swap.bydirection("up") },
    { rule = { class = "URxvt", instance = "profanity" },
      properties = { focus = true, switchtotag = true },
      		     callback = awful.client.setslave },
    { rule = { class = "URxvt", instance = "rc.lua" },
      properties = { switchtotag = true, focus = true } },
    { rule = { class = "URxvt", instance = "float" },
      properties = { placement = awful.placement.centered, focus = true, floating = true } },
    { rule = { class = "Conky" },
       properties = { floating = true, sticky = true, ontop = false,
                       focusable = false } }    
    -- END MY
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c)
    -- Set the windows at the slave,
    -- i.e. put it at the end of others instead of setting it master.
    -- if not awesome.startup then awful.client.setslave(c) end

    if awesome.startup and
      not c.size_hints.user_position
      and not c.size_hints.program_position then
        -- Prevent clients from being unreachable after screen count changes.
        awful.placement.no_offscreen(c)
    end
end)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal("request::titlebars", function(c)
    -- buttons for the titlebar
    local buttons = awful.util.table.join(
        awful.button({ }, 1, function()
            client.focus = c
            c:raise()
            awful.mouse.client.move(c)
        end),
        awful.button({ }, 3, function()
            client.focus = c
            c:raise()
            awful.mouse.client.resize(c)
        end)
    )

    awful.titlebar(c) : setup {
        { -- Left
            awful.titlebar.widget.iconwidget(c),
            buttons = buttons,
            layout  = wibox.layout.fixed.horizontal
        },
        { -- Middle
            { -- Title
                align  = "center",
                widget = awful.titlebar.widget.titlewidget(c)
            },
            buttons = buttons,
            layout  = wibox.layout.flex.horizontal
        },
        { -- Right
            awful.titlebar.widget.floatingbutton (c),
            awful.titlebar.widget.maximizedbutton(c),
            awful.titlebar.widget.stickybutton   (c),
            awful.titlebar.widget.ontopbutton    (c),
            awful.titlebar.widget.closebutton    (c),
            layout = wibox.layout.fixed.horizontal()
        },
        layout = wibox.layout.align.horizontal
    }
end)

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal("mouse::enter", function(c)
    if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
        and awful.client.focus.filter(c) then
        client.focus = c
    end
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}

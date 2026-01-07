--
-- ~/.config/nvim/lua/plugins/colorizer.lua
--

return {
  'norcalli/nvim-colorizer.lua',
  opts = {
    RGB = true, -- #RGB hex codes
    RRGGBB = true, -- #RRGGBB hex codes
    names = false, -- "Name" codes like Blue
    RRGGBBAA = true, -- #RRGGBBAA hex codes
    rgb_fn = false, -- CSS rgb() and rgba() functions
    hsl_fn = false, -- CSS hsl() and hsla() functions
    css = false, -- Enable all CSS features: rgb_fn, hsl_fn, names, RGB, RRGGBB
    css_fn = false, -- Enable all CSS *functions*: rgb_fn, hsl_fn
    -- Available modes: foreground, background
    mode = 'background', -- Set the display mode.
  },
  config = function()
    require('colorizer').setup()
  end,
}

-- vim: ts=2 sts=2 sw=2 et

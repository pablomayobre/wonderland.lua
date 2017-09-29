local animate = {
  _VERSION     = 'animate 0.0.0',
  _DESCRIPTION = 'Tweening for lua with keyframe chaining',
  _URL         = 'https://github.com/Positive07/animate.lua',
  _LICENSE     = [[
    MIT LICENSE

    Copyright (c) 2017 Pablo A. Mayobre (Positive07)

    Permission is hereby granted, free of charge, to any person obtaining a
    copy of this software and associated documentation files (the
    "Software"), to deal in the Software without restriction, including
    without limitation the rights to use, copy, modify, merge, publish,
    distribute, sublicense, and/or sell copies of the Software, and to
    permit persons to whom the Software is furnished to do so, subject to
    the following conditions:

    The above copyright notice and this permission notice shall be included
    in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
    OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
    IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
    CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
    TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
    SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
  ]]
}

local base = {
  quad  = function (x) return x^2 end,
  cubic = function (x) return x^3 end,
  quart = function (x) return x^4 end,
  quint = function (x) return x^5 end,
  sine = function (x) return 1 - math.cos(x * (math.pi / 2)) end,
  expo = function (x) return x == 0 and 0 or 2 ^ (10 * (x - 1)) end,
  circ = function (x) return 1 - math.sqrt(1 - x * x) end,
  elastic = function (x, tab)
    if x == 0 then
      return 0
    elseif x == 1 then
      return 1
    else
      local s, period, amplitude
      x, period = x - 1, (not tab.period or tab.period > 1) and 0.3 or tab.period
      if (tab.amplitude or 0) < 1 then
        amplitude = 1
        s = math.sin((x - period/4) * (2 * math.pi) / period)
      else
        amplitude = tab.amplitude
        s = math.sin(x * (2 * math.pi) / period - math.asin(1 / amplitude))
      end
      return -(amplitude * 2 ^ (10 * x) * s)
    end
  end,
  back = function (x, tab)
    local amount = tonumber(tab.amount) or 1.70158
    return x * x * ((amount + 1) * x - amount)
  end,
  bounce = function (x)
    if x < 1 / 2.75 then
      return 7.5625 * x^2
    elseif x < 2 / 2.75 then
      return 7.5625 * (x - (1.5 / 2.75))^2 + 0.75
    elseif x < 2.5 / 2.75 then
      return 7.5625 * (x - (2.25 / 2.75))^2 + 0.9375
    else
      return 7.5625 * (x - (2.625 / 2.75))^2 + 0.984375
    end
  end
}

local methods = {
  linear = function (x)
    return x
  end,
  delay = function () return 0 end,
  set   = function () return 0 end
}

for name, func in pairs(base) do
  methods[name..'-in'] = func

  methods[name..'-out'] = function (x, ...)
    x = 1 - x
    return func(x, ...)
  end

  methods[name..'-inout'] = function (x, ...)
    x = x * 2
    if x < 1 then
      return .5 * func(x, ...)
    else
      return 1 - func(2 - x, ...) * .5
    end
  end

  methods[name..'-outin'] = function (x, ...)
    x = x * 2
    if x < 1 then
      return .5 * (1 - func(1 - x, ...))
    else
      return .5 * (1 + func(x - 1, ...))
    end
  end
end


local hascubicbezier, cubicBezier = pcall(function ()
  return assert(love.math.newBezierCurve) --luacheck: std love+luajit
end)

local getCubicBezier = function (vert)
  if not hascubicbezier then
    error('Keyframe type, Cubic Beziers are not supported', 2)
  elseif #vert ~= 4 then
    error('Keyframe type, Cubic Bezier must be a table with 4 numbers', 2)
  end

  local vertices = {}
  for i=1, 4 do
    local v = tonumber(vert[i])

    if not v then
      error('Keyframe type, Cubic Bezier value #'..i..' expected a number, got a '..type(vert[i]), 2)
    end

    vertices[i] = v
  end

  local curve = cubicBezier(vertices)
  return function (x)
    return curve:evaluate(x)
  end
end

local playback, animation = {}, {}

local playbackmt  = {__index = playback }
local animationmt = {__index = animation}

function playback:getPosition() return self._position end

function playback:setPosition (position)
  self._position = math.max(position, 0)

  self._value = self._anim:evaluate(self._position, self._loop)

  return self._value, self:isCompleted()
end

function playback:update (dt)
  if not self._paused then
    return self:set(self._position + dt)
  else
    return self._value, self:isCompleted()
  end
end

function playback:getAnimation() return self._anim end

function playback:pause() self._paused = true end
function playback:play() self._paused = false end
function playback:isPaused() return self._paused end

function playback:isLooping () return self._loop end
function playback:setLooping (loop) self._loop = not not loop end

function playback:getValue () return self._value end
function playback:isCompleted ()
  return self._position == 0 or self._position >= self._anim:getLength()
end
function playback:reset ()
  self._paused = false
  self._position = 0
  self._value = self._anim:evaluate(0)
end

local getMethod = function (name)
  if type(name) == 'function' then
    return name
  end

  if type(name) == 'string' then
    if methods[name] then
      return methods[name]
    else
      error('Keyframe type, the method name is invalid', 3)
    end
  end

  if type(name) == 'table' then
    local ok, err = pcall(getCubicBezier, name)

    if not ok then
      error(err, 2)
    else
      return err
    end
  end

  if hascubicbezier then
    error('Keyframe type must be a string or a table representing a Cubic Bezier', 3)
  else
    error('Keyframe type must be a string, Cubic Beziers are not supported', 3)
  end
end

function animation:add (keyframe)
  local name = keyframe[1]

  local length, value = keyframe.length, keyframe.value

  if (not tonumber(length)) and name ~= 'set' then
    error('Keyframe length property (number expected, got '..type(length)..')', 2)
  elseif (not tonumber(value)) and name ~= 'delay' then
    error('Keyframe value property (number expected, got '..type(value)..')', 2)
  end

  getMethod(name)

  table.insert(self.keyframes, keyframe)

  self._length = self._length + length
  self._lastvalue = value

  return self
end

function animation:getLength ()
  return self._length
end

function animation:evaluate (position, loop)
  if position >= self._length then
    if not loop then
      return self._lastvalue
    else
      position = position % self._length
      position = position == 0 and self._length or position
    end
  end

  local start, initial = 0, self._initial
  for _, keyframe in ipairs(self.keyframes) do
    if keyframe[1] == 'set' then
      initial = tonumber(keyframe.value)
    end

    local delta = (position - start) / tonumber(keyframe.length)

    if delta >= 0 and delta <= 1 then
      if keyframe[1] == 'delay' then
        return initial
      end

      local method = getMethod(keyframe[1])
      local change = tonumber(keyframe.value) - initial

      return initial + method(delta, keyframe) * change
    else
      start = start + tonumber(keyframe.length)
      initial = tonumber(keyframe.value)
    end
  end

  return self._initial
end

function animation:newPlayback (loop)
  return setmetatable({
    _loop = loop,
    _anim = self,
    _position = 0,
    _value = self:evaluate(0),
    _completed = false,
    _paused = false
  }, playbackmt)
end

local new = function (start)
  local value = tonumber(start or 0)

  if not value then
    error('bad argument #1 to animate.new (number expected, got '..type(start)..')', 2)
  end

  return setmetatable({
    _initial = value,
    _lastvalue = value,
    _length = 0,
    keyframes = {}
  }, animationmt)
end

function animation:clone ()
  local clone = new(self._initial)

  for _, keyframe in self.keyframes do
    clone:add(keyframe)
  end

  return clone
end

animate.new = new --Main function!
animate.methods = methods
animate.getCubicBezier = getCubicBezier

return animate

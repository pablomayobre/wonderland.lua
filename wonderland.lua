local wonderland = {
  _VERSION     = 'wonderland 0.0.0',
  _DESCRIPTION = 'Tweening for lua with keyframe chaining',
  _URL         = 'https://github.com/Positive07/wonderland.lua',
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

local methods = {
  --Basic methods:
  linear = function (x) return x end,
  delay  = function ()  return 0 end,
  set    = function ()  return 0 end
}

do
  local base = {
    --Power methods:
    quad  = function (x) return x^2 end,
    cubic = function (x) return x^3 end,
    quart = function (x) return x^4 end,
    quint = function (x) return x^5 end,

    --Simple methods:
    sine  = function (x) return 1 - math.cos(x * (math.pi / 2)) end,
    expo  = function (x) return x == 0 and 0 or 2 ^ (10 * (x - 1)) end,
    circ  = function (x) return 1 - math.sqrt(1 - x * x) end,

    --Complex methods:
    elastic = function (x, tab)
      if x == 0 or x == 1 then return x end
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

  --"in", "out", "inout", "outin" variations of the above methods
  for name, func in pairs(base) do
    methods[name..'-in'] = func

    methods[name..'-out'] = function (x, ...)
      x = 1 - x
      return 1 - func(x, ...)
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
end

--Cubic Bezier support for LÖVE:
local hasCubicBezier, cubicBezier = pcall(function ()
  return assert(love.math.newBezierCurve) --luacheck: std love+luajit
end)

local function getCubicBezier (vert)
  if not hasCubicBezier then
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

--Type Checking:
local str = "bad argument #%s to '%s' (number expected, got %s)"
local function checknumber (func, arg, value)
  local v = tonumber(value)
  local typ = type(value)

  if not v then
    error(str:format(arg, func, typ), 3)
  end

  return v
end

local function getMethod (name)
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

  if hasCubicBezier then
    error('Keyframe type must be a string, function, or a table representing a Cubic Bezier', 3)
  else
    error('Keyframe type must be a string or function, Cubic Beziers are not supported', 3)
  end
end

--Playback Objects:
local playback = {}
playback.__index = playback

local function newPlayback (animation, loop)
  local play = {
    loop = loop,
    animation = animation,
    position = 0,
    value = animation:evaluate(0),
  }

  setmetatable(play, playback)

  return play
end

function playback:seek (position)
  position = checknumber('playback:seek', 1, position)

  self.position = math.max(position, 0)
  self.value = self.animation:evaluate(self.position, self.loop)
end

function playback:tell ()
  return self.value, self:isCompleted()
end

function playback:update (dt)
  dt = checknumber('playback:update', 1, dt)

  self:seek(self.position + dt)

  return self:tell()
end

function playback:isCompleted ()
  return self.position == 0 or self.position >= self.animation:getLength()
end

function playback:reset ()
  self.position = 0
  self.value = self.animation:evaluate(0)
end

--Animation Objects:
local animation = {}
animation.__index = animation

local function newAnimation (start)
  start = checknumber('wonderland.new', 1, start or 0)

  local anim = {
    startValue = start,
    finalValue = start,
    length = 0,
    keyframes = {}
  }

  setmetatable(anim, animation)

  return anim
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

  self.length = self.length + length
  self.finalValue = value

  return self
end

function animation:evaluate (position, loop)
  if position >= self.length then
    if not loop then
      return self.finalValue
    else
      position = position % self.length
      position = position == 0 and self.length or position
    end
  end

  local start, initial = 0, self.startValue
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

  return self.startValue
end

function animation:clone ()
  local clone = newAnimation(self.startValue)

  for _, keyframe in self.keyframes do
    clone:add(keyframe)
  end

  return clone
end

function animation:newPlayback (loop)
  return newPlayback(self, loop)
end

--Exposed functions:
wonderland.new = newAnimation --Main function!
wonderland.methods = methods
wonderland.getCubicBezier = getCubicBezier

return setmetatable(wonderland, {__call = function (_, ...) return newAnimation(...) end})

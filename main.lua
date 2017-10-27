--luacheck: std love+luajit

local animation, playback

local methods = {
  'linear',
  'quad-in',    'quad-out',    'quad-inout',    'quad-outin',
  'cubic-in',   'cubic-out',   'cubic-inout',   'cubic-outin',
  'quart-in',   'quart-out',   'quart-inout',   'quart-outin',
  'quint-in',   'quint-out',   'quint-inout',   'quint-outin',
  'sine-in',    'sine-out',    'sine-inout',    'sine-outin',
  'expo-in',    'expo-out',    'expo-inout',    'expo-outin',
  'circ-in',    'circ-out',    'circ-inout',    'circ-outin',
  'elastic-in', 'elastic-out', 'elastic-inout', 'elastic-outin',
  'back-in',    'back-out',    'back-inout',    'back-outin',
  'bounce-in',  'bounce-out',  'bounce-inout',  'bounce-outin',
}

love.load = function ()
  local wonderland = require('wonderland.wonderland')

  animation = wonderland.new(0)

  for _, method in ipairs(methods) do
    animation:add{method, length = 1, value = 1}
    animation:add{method, length = 1, value = 0}
  end

  playback = animation:newPlayback(true)
end

local method, value = 'linear', 0

love.update = function (dt)
  value = playback:update(dt)

  local position = playback:getPosition() / 2
  local index = math.min(math.max(math.ceil(position), 1), #methods)

  method = methods[index]
end

local circle = function (x, y, r)
  love.graphics.circle('fill', x, y, r)
  love.graphics.circle('line', x, y, r)
end

love.draw = function ()
  love.graphics.setColor(255, 255, 255, 255)
  love.graphics.printf(method, 0, 5, 250, 'center')

  love.graphics.setColor(255, 255, 255, value * 255)
  circle(125,130,100)

  love.graphics.setColor(255, 0, 0, 255)
  circle(45 + value * 140, 280, 20)
end

# [Animate][animate]

Animate is a simple tweening library for Lua.

[Hump.timer][hump] is as simple as a tweening library can get, while [Flux][flux] and [Tween][tween] are too abstracted away, modifying multiple properties inside a table .

Neither [Hump][hump] nor [Tween][tween] offer a simple way to chain tweens like [Flux][flux] does with `tween:after()`.

Animate is the middle ground that tries to address some of this problems.

> You should still check the other libs, deciding between them depends on taste and specific use case

## Usage

> Animate doesn't have any dependencies, but it should be noted that it doesn't implement or include any kind of internal clock, so you either need a module to do that or your framework should be able to give you time deltas.

So to use the library, drop `animate.lua` somewhere and require it
```lua
local animate = require 'animate'
```

You can then create a new `animation`, and start adding keyframes
```lua
local animation = animate.new(10) -- Initial value 10
  :add{'delay', duration = 10}
  :add{'linear', duration = 4, value = 12}
  :add{'cubic-inout', duration = 3, value = 10}
```

Generally you want to create a `progress` object from your `animation`
```lua
local progress = animation:progress()
```

Then in your main loop you would call `progress:update(dt)` to get the new value of the animation.

Check the [wiki][wiki] for information on what each function does.

## License

Animate is licensed under the terms of the [MIT License][mit].
Copyright (c) 2017 Pablo A. Mayobre ([Positive07][positive])

[animate]: https://github.com/Positive07/animate.lua
[hump]: hump.readthedocs.io/en/latest/timer.html
[tween]: https://github.com/kikito/tween.lua
[flux]: https://github.com/rxi/flux
[wiki]: https://github.com/Positive07/animate.lua/wiki
[mit]: https://github.com/Positive07/animate.lua/blob/master/LICENSE
[positive]: https://github.com/Positive07

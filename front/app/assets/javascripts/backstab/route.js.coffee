goog.provide("backstab.Route")

goog.require('lime.Layer')
goog.require('lime.Sprite')

class backstab.Route
  constructor: (from, to) ->
    @source = from
    @dest = to
    from.neighbors.push to
    to.neighbors.push from

  draw: ->
    from = [@source.x, @source.y]
    to = [@dest.x, @dest.y]
    delta = 15
    x1 = to[0] - from[0]
    y1 = to[1] - from[1]
    x2 = 100
    y2 = 0
    cosFi = (x1 * x2 + y1 * y2) / Math.sqrt(x1 * x1 + y1 * y1) / Math.sqrt(x2 * x2 + y2 * y2)
    fi = Math.acos(cosFi) / Math.PI * 180
    area = x1 * y2 - x2 * y1
    sign = area / Math.abs(area)
    length = Math.sqrt(Math.pow(to[0] - from[0], 2) + Math.pow(to[1] - from[1], 2))
    width = 20
    node = new lime.Sprite().setSize(length, width).setFill(150, 200, 0).setAnchorPoint(0, .5).
      setPosition(from[0], from[1]).setRotation(fi * sign)
    node


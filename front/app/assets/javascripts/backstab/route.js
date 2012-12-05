goog.provide("backstab.Route");

goog.require('lime.Layer');
goog.require('lime.Sprite');

backstab.Route = function(from, to) {
  this.source = from;
  this.dest = to;
  from.neighbors.push(to);
  to.neighbors.push(from);
};

backstab.Route.prototype.draw = function() {
  // вычисление градуса, на который надо повернуть спрайт, чтобы было красиво
  var from = [this.source.x, this.source.y];
  var to = [this.dest.x, this.dest.y];
  var delta = 15;
  var x1 = to[0] - from[0],
      y1 = to[1] - from[1],
      x2 = 100,
      y2 = 0;
  var cosFi = (x1 * x2 + y1 * y2) /
    Math.sqrt(x1 * x1 + y1 * y1) /
    Math.sqrt(x2 * x2 + y2 * y2);
  var fi = Math.acos(cosFi) / Math.PI * 180;
  var area = x1*y2 - x2*y1;
  var sign = area / Math.abs(area)

  var length = Math.sqrt(Math.pow(to[0] - from[0], 2) + Math.pow(to[1] - from[1], 2)),
      width = 20;
  var node = new lime.Sprite().setSize(length, width).setFill(150, 200, 0).
    setAnchorPoint(0, .5).setPosition(from[0], from[1]).setRotation(fi * sign);
  return node;
};


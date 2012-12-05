goog.provide("backstab.Planet");

goog.require('lime.Layer');
goog.require('lime.Circle');
goog.require('lime.Label');
goog.require('lime.animation.ColorTo');

backstab.Planet = function(params) {
  this.neighbors = [];
  this.id = params[1].value;
  this.type = params[2].value;
  this.userId = params[3].value;
  this.quantity = params[4];
  this.capacity = params[5];
};

backstab.Planet.prototype.draw = function() {
  var planet = new lime.Layer().setPosition(this.x, this.y),
      circle = new lime.Circle().setSize(150,150).setFill(255,150,0),
      base,
      lbl = new lime.Label().setSize(160,50).setFontSize(30).setText(this.id + '');
  circle.appendChild(lbl);
  planet.appendChild(circle);

  planet.object = this;
  this.node = planet;

  planet.showDropHighlight = function(){
    circle.runAction(new lime.animation.ColorTo(255,198,201).setDuration(.3));
  };
  planet.hideDropHighlight = function(){
    circle.runAction(new lime.animation.ColorTo(255,150,0).setDuration(.1));
  };
  return planet;
};

backstab.Planet.prototype.drawBase = function() {
  // create planetbase
  var color;
  if (this.userId === backstab.userinfo.id) {
    color = [121,142,224];
  } else {
    color = [201,32,32];
  }
  var base = new lime.Sprite().setPosition(this.node.getPosition()).setSize(70, 70).setFill(color[0], color[1], color[2]);
  var handler = new lime.Sprite().setSize(base.getSize()).setFill(255, 0, 0).setOpacity(.01);
  var text = new lime.Label().setFontSize(30).setText(this.quantity);
  base.appendChild(handler);
  base.appendChild(text);

  // Drag army to another planet
  goog.events.listen(handler,['mousedown','touchstart'],goog.bind(function(e){
    e.event.stopPropagation();
    handler.runAction(new lime.animation.Spawn(
      new lime.animation.FadeTo(.8).setDuration(.2),
      new lime.animation.ScaleTo(1.3).setDuration(.2)
    ));
    var drag = e.startDrag();
    this.neighbors.forEach(function(planet) {
      drag.addDropTarget(planet.node);
    });

    goog.events.listen(drag, lime.events.Drag.Event.CANCEL, function(){
      handler.runAction(new lime.animation.Spawn(
        new lime.animation.MoveTo(0, 0).setDuration(.2),
        new lime.animation.FadeTo(.01).setDuration(.2),
        new lime.animation.ScaleTo(1).setDuration(.2)
      ));
    });

    goog.events.listen(drag, lime.events.Drag.Event.DROP, goog.bind(function(e) {
      //var dropTarget = e.activeDropTarget;
      backstab.send(Bert.tuple(Bert.atom("goto"), [this.id + '', e.activeDropTarget.object.id + '']));
      e.stopPropagation();
      handler.runAction(new lime.animation.Sequence(
        new lime.animation.Spawn(
          new lime.animation.FadeTo(.01).setDuration(.2),
          new lime.animation.ScaleTo(1).setDuration(.2)
        ),
        new lime.animation.MoveTo(0,0).setDuration(.1)
      ));
    }, this));
  }, this));
  return base;
};

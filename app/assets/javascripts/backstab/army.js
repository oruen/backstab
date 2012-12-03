goog.provide("backstab.Army");

goog.require('lime.Layer');
goog.require('lime.Sprite');
goog.require('lime.animation.Spawn');
goog.require('lime.animation.Sequence');
goog.require('lime.animation.FadeTo');
goog.require('lime.animation.MoveTo');
goog.require('lime.animation.ScaleTo');

backstab.Army = function(params, battleMap) {
  this.type = params[1].value;
  this.quantity = parseInt(params[2].value, 10);
  this.planetId = parseInt(params[3].value, 10);
  this.userId = parseInt(params[4].value, 10);
  this.battleMap = battleMap;
};

backstab.Army.prototype.planet = function() {
  return this.battleMap.planetById(this.planetId);
};

backstab.Army.prototype.draw = function() {
  var planet = this.planet(),
      color;
  if (this.userId === backstab.userinfo.id) {
    color = [121,142,224];
  } else {
    color = [201,32,32];
  }
  var node = new lime.Layer().setPosition(planet.x, planet.y)
  var starship = new lime.Sprite().setSize(70, 70).setFill(color[0], color[1], color[2]);
  var text = new lime.Label().setFontSize(30).setText(this.quantity);
  var handler = new lime.Sprite().setSize(starship.getSize()).setFill(255, 0, 0).setOpacity(.01);
  node.appendChild(handler);
  node.appendChild(starship);
  node.appendChild(text);

  // Drag army to another planet
  goog.events.listen(handler,['mousedown','touchstart'],goog.bind(function(e){
    e.event.stopPropagation();
    handler.runAction(new lime.animation.Spawn(
      new lime.animation.FadeTo(.8).setDuration(.2),
      new lime.animation.ScaleTo(1.3).setDuration(.2)
    ));
    var drag = e.startDrag();
    planet.neighbors.forEach(function(planet) {
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
      backstab.send(["goto", this.planetId, e.activeDropTarget.object.id]);
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

  return node;
};

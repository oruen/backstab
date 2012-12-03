goog.provide("backstab.EventHandler");

backstab.EventHandler = function(wsHandler, scene) {
  wsHandler.addEventListener(backstab.WsHandler.EventType.MAP, function (event) {
    console.log("Time to draw");
    backstab.currentMap = new backstab.BattleMap(event.target, scene);
    backstab.currentMap.render();
  });

  wsHandler.addEventListener(backstab.WsHandler.EventType.USERINFO, function(event) {
    console.log("Userinfo", event.target);
    backstab.userinfo = new backstab.User(event.target);
  });

  wsHandler.addEventListener(backstab.WsHandler.EventType.GOTO, function(event) {
    console.log("Goto", event.target);
    var from = backstab.currentMap.planetById(event.target[0]),
        to = backstab.currentMap.planetById(event.target[1]);
    if (from.userId === backstab.userinfo.id) {
      color = [121,142,224];
    } else {
      color = [201,32,32];
    }
    var army = new lime.Circle().setPosition(from.node.getPosition()).setSize(80, 80).setFill(color[0], color[1], color[2]);
    scene.appendChild(army);
    var anim = new lime.animation.Sequence(
      new lime.animation.MoveTo(to.node.getPosition()),
      new lime.animation.FadeTo(0).setDuration(.2)
    );
    goog.events.listen(anim, "stop", goog.bind(function() {
      scene.removeChild(army);
    }, backstab));
    army.runAction(anim);
  });
};


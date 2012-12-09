goog.provide("backstab.EventHandler")

backstab.EventHandler = (wsHandler) ->
  wsHandler.addEventListener backstab.WsHandler.EventType.MAP, (event) ->
    console.log "Time to draw"
    backstab.currentMap = new backstab.BattleMap(event.target, backstab.scene)
    backstab.currentMap.render()

  wsHandler.addEventListener backstab.WsHandler.EventType.GLOBAL_MAP, (event) ->
    console.log "Global map"
    backstab.planetSystems = []
    backstab.players = {}
    event.target[0].forEach (e) ->
      player = new backstab.Player(e)
      backstab.players[player.id] = player
    event.target[1].forEach (e) ->
      backstab.planetSystems.push new backstab.PlanetSystem(e)

    backstab.renderGlobalMap backstab.planetSystems

  wsHandler.addEventListener backstab.WsHandler.EventType.GOTO, (event) ->
    console.log "Goto", event.target
    from = backstab.currentMap.planetById(event.target[0])
    to = backstab.currentMap.planetById(event.target[1])
    if from.userId is backstab.userinfo.id
      color = [121, 142, 224]
    else
      color = [201, 32, 32]
    army = new lime.Circle().setPosition(from.node.getPosition()).setSize(80, 80).setFill(color[0], color[1], color[2])
    scene.appendChild army
    anim = new lime.animation.Sequence(new lime.animation.MoveTo(to.node.getPosition()), new lime.animation.FadeTo(0).setDuration(.2))
    goog.events.listen anim, "stop", goog.bind(->
      scene.removeChild army
    , backstab)
    army.runAction anim


goog.provide('backstab.BattleMap');

goog.require('backstab.Planet');
goog.require('backstab.Route');
goog.require('backstab.Army');

backstab.BattleMap = function(mapInfo, scene) {
  this.scene = scene;
  this.planets = [];
  this.routes = [];

  this.createPlanets(mapInfo);
  this.createRoutes(mapInfo);
};

backstab.BattleMap.prototype.render = function(scene) {
  var scene = this.scene;
  this.routes.forEach(function(obj) {
    scene.appendChild(obj.draw());
  });
  this.planets.forEach(goog.bind(function(obj) {
    scene.appendChild(obj.draw());
  }, this));
  this.planets.filter(function(e) {return !!e.userId;}).forEach(goog.bind(function(obj){
    scene.appendChild(obj.drawBase());
  }, this));
};

backstab.BattleMap.prototype.createPlanets = function(mapInfo) {
  var planetsInfo = mapInfo.filter(function(e) {return e[0].value === "planets"})[0][1].map(function(e) {return e.value;});
  planetsInfo.forEach(goog.bind(function(params) {
    console.log("planet", params);
    var planet = new backstab.Planet(params);
    this.planets.push(planet);
  }, this));
};

backstab.BattleMap.prototype.createRoutes = function(mapInfo) {
  var routesInfo = mapInfo.filter(function(e) {return e[0].value === "routes"})[0][1].map(function(e) {return e.value;});
  var from, to;
  routesInfo.forEach(goog.bind(function(params) {
    console.log("route", params);
    from = this.planets.filter(function(p) {return p.id == params[1].value})[0];
    to = this.planets.filter(function(p) {return p.id == params[2].value})[0];
    var route = new backstab.Route(from, to);
    this.routes.push(route);
  }, this));
};

backstab.BattleMap.prototype.planetById = function(id) {
  return this.planets.filter(function(e) { return e.id === id})[0];
};

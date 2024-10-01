//adapted from
//https://gsap.com/community/forums/topic/17157-drawing-drag-and-drop-connectors-between-svg-elements/?do=findComment&comment=76819

SVGElement.prototype.getTransformToElement = 
  SVGElement.prototype.getTransformToElement || 
  function (toElement) {
    return toElement.getScreenCTM().inverse().multiply(this.getScreenCTM());
  };

//
// CONNECTOR
// ===========================================================================
class Connector {

  constructor() {

    this.id = `connector_${++nextUid}`;
    this.dragType = "connector";
    this.isSelected = false;
    this.element = connectorElement.cloneNode(true);
    this.path = this.element.querySelector(".connector-path");
    this.pathOutline = this.element.querySelector(".connector-path-outline");
    this.inputHandle = this.element.querySelector(".input-handle");
    this.outputHandle = this.element.querySelector(".output-handle");
  }

  init(port) {
    connectorLayer.appendChild(this.element);
    this.setStaticPort(port);

  }

  setStaticPort(port, nonStaticPort = null){
    this.isInput = port.isInput;

    if (port.isInput) {
      this.inputPort = port;
      this.dragElement = this.outputHandle;
      this.staticElement = this.inputHandle;
    } else {
      this.outputPort = port;
      this.dragElement = this.inputHandle;
      this.staticElement = this.outputHandle;
    }

    this.staticPort = port;
    this.dragElement.setAttribute("data-drag", `${this.id}:connector`);
    this.staticElement.setAttribute("data-drag", `${port.id}:port`);

    if(nonStaticPort == null){
      TweenLite.set([this.inputHandle, this.outputHandle], {
        x: port.global.x,
        y: port.global.y
      });
    }else{
      TweenLite.set([this.outputHandle], 
        nonStaticPort.isInput 
        ? { x: port.global.x, y: port.global.y} 
        : { x: nonStaticPort.global.x, y: nonStaticPort.global.y}
      );
      TweenLite.set([this.inputHandle], 
        nonStaticPort.isInput 
        ? { x: nonStaticPort.global.x, y: nonStaticPort.global.y} 
        : { x: port.global.x, y: port.global.y}
      );
    }

    
  }

  updatePath() {

    const x1 = this.inputHandle._gsTransform.x;
    const y1 = this.inputHandle._gsTransform.y;

    const x4 = this.outputHandle._gsTransform.x;
    const y4 = this.outputHandle._gsTransform.y;

    const dx = Math.abs(x1 - x4) * bezierWeight;

    const p1x = x1;
    const p1y = y1;

    const p2x = x1 - dx;
    const p2y = y1;

    const p4x = x4;
    const p4y = y4;

    const p3x = x4 + dx;
    const p3y = y4;

    const data = `M${p1x} ${p1y} C ${p2x} ${p2y} ${p3x} ${p3y} ${p4x} ${p4y}`;

    this.path.setAttribute("d", data);
    this.pathOutline.setAttribute("d", data);
  }

  updateHandle(port) {

    if (port === this.inputPort) {

      TweenLite.set(this.inputHandle, {
        x: port.global.x,
        y: port.global.y
      });


    } else if (port === this.outputPort) {

      TweenLite.set(this.outputHandle, {
        x: port.global.x,
        y: port.global.y
      });

    }

    this.updatePath();
  }

  placeHandle() {
    const skipShape = this.staticPort.parentNode.element;

    let hitPort;

    for (let shape of shapes) {

      if (shape.element === skipShape) {
        continue;
      }

      if (Draggable.hitTest(this.dragElement, shape.element)) {

        const ports = this.isInput ? shape.outputPorts : shape.inputPorts;

        for (let port of ports) {

          if (Draggable.hitTest(this.dragElement, port.portElement)) {
            hitPort = port;
            break;
          }
        }

        if (hitPort) {
          break;
        }
      }
    }

    if (hitPort) {

      
      if (this.isInput) {
        this.outputPort = hitPort;
      } else {
        this.inputPort = hitPort;
      }

      this.dragElement.setAttribute("data-drag", `${hitPort.id}:port`);

      hitPort.addConnector(this);
      this.updateHandle(hitPort);

      this.inputPort.parentNode.onConnect(this);

    } else {
      this.remove();
    }
  }


  remove() {

    if (this.inputPort) {
      this.inputPort.removeConnector(this);
    }

    if (this.outputPort) {
      this.outputPort.removeConnector(this);
    }

    this.isSelected = false;

    this.path.removeAttribute("d");
    this.pathOutline.removeAttribute("d");
    this.dragElement.removeAttribute("data-drag");
    this.staticElement.removeAttribute("data-drag");

    this.staticPort = null;
    this.inputPort = null;
    this.outputPort = null;
    this.dragElement = null;
    this.staticElement = null;

    connectorLayer.removeChild(this.element);
    connectorPool.push(this);
  }

  onDrag() {
    this.updatePath();
  }

  onDragEnd() {
    this.placeHandle();
  }
}


//
// NODE PORT
// =========================================================================== 
class NodePort {

  constructor(parentNode, isInput, i, label, subtitle, y) {

    this.id = `port_${++nextUid}`;
    this.dragType = "port";
    
    this.parentNode = parentNode;
    this.isInput = isInput;
    this.i = i;
    this.label = label;
    this.subtitle = subtitle;

    this.element = isInput ? inputFieldElement.cloneNode(true) : outputFieldElement.cloneNode(true);

    this.element.setAttribute("transform", `translate(0, ${y})`);
    this.element.querySelector(".port-label").innerHTML = label;
    this.element.querySelector(".port-subtitle").innerHTML = subtitle;

    this.parentNode.element.querySelector(isInput ? ".inputs" : ".outputs").appendChild(this.element);

    this.portElement = this.element.querySelector(".port");
    this.portScrim = this.element.querySelector(".port-scrim");

    this.portScrim.setAttribute("data-drag", `${this.id}:port`);

    this.connectors = [];
    this.lastConnector;

    const bbox = this.portElement.getBBox();

    this.global = svg.createSVGPoint();
    this.center = svg.createSVGPoint();
    this.center.x = bbox.x + bbox.width / 2;
    this.center.y = bbox.y + bbox.height / 2;

    this.update();
  }

  getConnector() {

    let connector;

    if(this.isInput && this.connectors.length != 0){
      connector = this.connectors[0];
      connector.inputPort.removeConnector(connector);
      connector.setStaticPort(connector.outputPort, connector.inputPort);
    }else{

      if (connectorPool.length) {
        connector = connectorPool.pop();
        connectorLookup[connector.id] = connector;
      } else {
        connector = new Connector();
      }
      connector.init(this);
      this.connectors.push(connector);
    }

    this.lastConnector = connector;
  }

  removeConnector(connection) {

    const index = this.connectors.indexOf(connection);

    if (index > -1) {
      this.connectors.splice(index, 1);
    }
  }

  addConnector(connection) {
    if(this.isInput){
      this.connectors.forEach(c => c.remove());
    }
    this.connectors.push(connection);
  }

  update() {

    const transform = this.portElement.getTransformToElement(diagramElement);
    this.global = this.center.matrixTransform(transform);

    for (let connector of this.connectors) {
      connector.updateHandle(this);
    }
  }
}


//
// NODE SHAPE
// =========================================================================== 
class NodeShape {

  constructor(posElement, template) {
    this.posElement = posElement;
    this.template = template;
    this.menuData = structuredClone(template.menu);

    this.id = `shape_${++nextUid}`;
    this.dragType = "shape";

    this.inputPorts = [];
    this.outputPorts = [];

    this.element = nodeContainerElement.cloneNode(true);
    this.element.querySelector(".node-header").style.fill = template.color;
    nodeLayer.appendChild(this.element);

    this.element.setAttribute("data-drag", `${this.id}:shape`);

    let offset = 50;
    let height = 50;
  
    this.element.querySelector(".header-title").innerHTML = template.label;
  
    let inPortCount = template.inPorts.length;
    let inGap = inPortCount <= 1 ? 0 : height / (inPortCount - 1);
    for(let i = 0; i < inPortCount; i++){
      const port = new NodePort(this, true, i, template.inPorts[i].label, template.inPorts[i].subtitle, offset + i * inGap);
      portLookup[port.id] = port;
      ports.push(port);
      this.inputPorts.push(port);
    }
    
  
    let outPortCount = template.outPorts.length;
    let outGap = outPortCount <= 1 ? 0 : height / (outPortCount - 1);
    for(let i = 0; i < outPortCount; i++){
      const port = new NodePort(this, false, i, template.outPorts[i].label, template.outPorts[i].subtitle, offset + i * outGap);
      portLookup[port.id] = port;
      ports.push(port);
      this.outputPorts.push(port);
    }

    this.element.addEventListener("contextmenu", (e) => {
      if(this.posElement.parentNode.classList.contains("crow")){
        removeCrow(this.posElement);
        this.remove();
        e.preventDefault();
      }
    });

    shapeLookup[this.id] = this;
    shapes.push(this);
  }

  update(){
    let {x, y} = this.posElement.getBoundingClientRect();
    let {width: ewidth, height: eheight} = this.element.getBoundingClientRect();

    let {x: areaX, y: areaY} = composeArea.getBoundingClientRect();

    TweenLite.set(this.element, { 
      x: x - ewidth / 2 - areaX, 
      y: y - eheight / 2 - areaY
    });

    for (let input of this.inputPorts) {
      input.update();
    }

    for (let output of this.outputPorts) {
      output.update();
    }
  }

  remove(){
    this.inputPorts.forEach(function(x) { 
      x.connectors.forEach(c => c.remove())
      portLookup[x.id] = null;
      x.parentNode = null;
      let index = ports.indexOf(x);
      if (index > -1) { 
        ports.splice(index, 1); 
      }
    });

    this.outputPorts.forEach(function(x) { 
      x.connectors.forEach(c => c.remove())
      portLookup[x.id] = null;
      x.parentNode = null;
      let index = ports.indexOf(x);
      if (index > -1) { 
        ports.splice(index, 1); 
      }
    });

    nodeLayer.removeChild(this.element);
    shapeLookup[this.id] = null;
    let i = shapes.indexOf(this);
    if (i > -1) {
      shapes.splice(i, 1); 
    }
  }

  onConnect(connection){
    //console.log(connection.inputPort, connection.outputPort);
  }

  onMenuClose(){

  }

}

//
// MENU
//===========================================================================
function showMenu(shape){
  if(shape.menuData.length == 0) return;
  if(menuHolder == shape){
    hideMenu();
    return;
  }
  shape.posElement.appendChild(menuPopupElement);
  clearMenu();

  //populate menu
  shape.menuData.forEach(function(x){
    let g = getMenuInputTypeElement(x.type, x.label, x.info, x.data);
    
    menuPopupElement.appendChild(g);
  })

  menuPopupElement.style.visibility = "visible";
  menuHolder = shape;
}

function hideMenu(){
  menuPopupElement.style.visibility = "hidden";

  //save data
  Array.from(menuPopupElement.children).forEach((element, index) => {
    switch(menuHolder.menuData[index].type){
      case "txtbox":
        menuHolder.menuData[index].data = element.lastElementChild.value;
        break;
      case "radio":
        let children = element.lastElementChild.children;
        for(let i = 0; i < children.length; i++){
          if(children[i].firstElementChild.checked){
            menuHolder.menuData[index].data = menuHolder.menuData[index].info[i].value;
            break;
          }
        }
        break;
      default:
        throw new Error(`Invalid type ${type}`);
    }
  });

  menuHolder = null;
}

function clearMenu(){
  menuPopupElement.innerHTML = '';
}

function getMenuData(shape, i){
  switch(shape.menuData[i].type){
    case "txtbox":
      return shape.menuData[i].data;
    case "radio":
      return shape.menuData[i].data;
    default:
      throw new Error(`Invalid type ${shape.menuData[i].type}`);
  }
}

function getMenuInputTypeElement(type, label, info, data){
  switch(type){
    case "txtbox":
      let txtg = document.createElement("div");
      txtg.classList.add("menu-group");

      let txtl = document.createElement("label");
      txtl.classList.add("menu-label");
      txtl.innerHTML = label;

      let txti = document.createElement("input");
      txti.classList.add("menu-input");
      txti.value = data;

      txtg.appendChild(txtl);
      txtg.appendChild(txti);

      return txtg;
    case "radio":
      let radg = document.createElement("div");
      radg.classList.add("menu-group");

      let radl = document.createElement("label");
      radl.classList.add("menu-label");
      radl.innerHTML = label;

      let radfs = document.createElement("fieldset");

      info.forEach((elem, i) => {
        let d = document.createElement("div");
        d.style.display = "inline-block"

        let l = "menu" + elem.label;

        let radi = document.createElement("input");
        radi.classList.add("menu-input");
        radi.type = "radio"
        radi.checked = elem.value == data;
        radi.value = elem.value;
        radi.id = l;
        radi.name = "menu" + label

        let radll = document.createElement("label");
        radll.innerHTML = elem.label;
        radll.setAttribute("for", l);

        d.appendChild(radi);
        d.appendChild(radll);

        radfs.appendChild(d);
      })

      radg.appendChild(radl);
      radg.appendChild(radfs);

      return radg
    default:
      throw new Error(`Invalid type ${type}`);
  }
  
}

//
// DIAGRAM
// ===========================================================================
class Diagram {

  constructor() {

    this.dragElement = this.element = diagramElement;
    this.target = null;
    this.targetDragType = null;
    this.isDragging = false;

    this.dragTarget = this.dragTarget.bind(this);
    this.prepareTarget = this.prepareTarget.bind(this);
    this.stopDragging = this.stopDragging.bind(this);

    this.draggable = new Draggable(dragProxy, {
      allowContextMenu: true,
      trigger: svg,
      onDrag: this.dragTarget,
      onDragEnd: this.stopDragging,
      onPress: this.prepareTarget
    });

  }

  stopDragging() {
    this.target && this.target.onDragEnd && this.target.onDragEnd();
    this.isDragging = false;
  }

  prepareTarget(event) {

    let element = event.target;
    let drag;

    while (!(drag = element.getAttribute("data-drag")) && element !== svg) {
      element = element.parentNode;
    }

    drag = drag || "diagram:diagram";
    const split = drag.split(":");
    this.dragId = split[0];
    this.dragType = split[1];

    if(this.dragType == "shape"){
      showMenu(shapeLookup[this.dragId]);
    }else{
      if(menuHolder != null){ hideMenu(); }
    }
  }

  startDrag(){
    switch (this.dragType) {
      case "port":
        const port = portLookup[this.dragId];
        port.getConnector();
        this.target = port.lastConnector;
        this.targetDragType = this.target.dragType;
        break;

      case "connector":
        this.target = connectorLookup[this.dragId];
        break;

      default:
        this.target = null;
        break;
    }
    this.isDragging = true;
  }

  dragTarget() {
    //this is necessary to prevent the creation of the connector on case that the user only clicks but doesnt drag
    if(this.isDragging == false) this.startDrag();

    if(this.target == null) return;

    TweenLite.set(this.target.dragElement, {
      x: `+=${this.draggable.deltaX}`,
      y: `+=${this.draggable.deltaY}`
    });


    this.target.onDrag && this.target.onDrag();
  }
}


//
// API
// ===========================================================================

function createNodeShape(template, posElement) {
  const shape = new NodeShape(posElement, template);
}

function updateConnections(){
  shapes.forEach(s => s.update());
}

function getComposition(){

  let c = getCompositionLoop(outputNodeShape.inputPorts[0]);
  let r = "";
  c.forEach(e => r += `${e.replace(/\//g, '\\/')}/`);
  return r;
}

function getCompositionLoop(port){
  
  //ignoring the case that not all input parameters are inserted for now
  if(port.connectors.length == 0){
    return ["null"];
  }

  let fromPort = port.connectors[0].outputPort;
  let shape = fromPort.parentNode;
  let comp = shape.template.outPorts[fromPort.i].comp;
  let res = [];

  for(let i = comp.length - 1; i >= 0; i--){
    let r = getCompositionParam(shape, comp[i]);
    res = res.concat(r)
  }
  console.log(res)
  return res;
}

function getCompositionParam(shape, compElement){
  switch(compElement.type){
    case "constant":
      return [compElement.value];
    case "inputPort":
      return getCompositionLoop(shape.inputPorts[compElement.value]);
    case "menuData":
      let r = "";
      compElement.value.forEach(e => {
        r += getMenuData(shape, e);
      });
      return r;
    default:
      throw new Error(`Invalid type ${compElement.type}`);

  }
}

//
// APP
// ===========================================================================
let nextUid = 0;

const bezierWeight = 0.675;

const svg = document.querySelector("#svg");
const diagramElement = document.querySelector("#diagram");

const shapeLookup = {};
const portLookup = {};
const connectorLookup = {};

const ports = [];
const shapes = [];
const connectorPool = [];

const dragProxy = document.querySelector("#drag-proxy");

const frag = document.createDocumentFragment();
frag.appendChild(document.querySelector(".connector"));
frag.appendChild(document.querySelector(".node-container"));
frag.appendChild(document.querySelector(".input-field"));
frag.appendChild(document.querySelector(".output-field"));
frag.appendChild(document.querySelector(".menu-group"));
const connectorElement = frag.querySelector(".connector");
const nodeContainerElement = frag.querySelector(".node-container");
const inputFieldElement = frag.querySelector(".input-field");
const outputFieldElement = frag.querySelector(".output-field");
const menuGroupElement = frag.querySelector(".menu-group");

const composeArea = document.querySelector('.compose-area');
const connectorLayer = document.querySelector("#connections-layer");
const nodeLayer = document.querySelector("#node-layer");

const menuPopupElement = document.querySelector(".menu-container");
let menuHolder = null;

const diagram = new Diagram();

const inputNodeShape = new NodeShape(document.querySelector(".srow-input"), 
  { label: "input"
  , color: "#d63c3c"    
  , menu: []
  , inPorts: []
  , outPorts: [{subtitle: "string", label: "input", comp: [ { type: "constant", value: "x1" } ] }]
  });

const outputNodeShape = new NodeShape(document.querySelector(".srow-output"), 
  { label: "output"
  , color: "#d63c3c"
  , menu: []
  , inPorts: [{subtitle: "value", label: "output"}]
  , outPorts: []
  });
  
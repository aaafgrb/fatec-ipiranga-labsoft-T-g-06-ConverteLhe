export function getCompShare(outNode, processMap){
  let m = new Map();
  getCompShareLoop(outNode, processMap, m)
  let r = Array.from(m).map(([k, v]) => v).join('|')
  return r
}

function getCompShareLoop(node, processMap, acc){
  let nodeId = node.processData.identifier
  if(acc.has(nodeId)){
    return;
  }
  let processId = node.processData.process.identifier
  let data = node.processData.data
  let connections = node.processData.inPorts

  let dataStr = Array.from(data).map(x => x.toString().replace(/[\/,|\\:]/g, m => `\\${m}`)).join(',')
  let connectionsStr = connections.map(x => x ? `${x.identifier}:${x.portIndex}` : "").join(',')

  acc.set(nodeId, `${nodeId}/${processId}/${dataStr}/${connectionsStr}`)
  node.processData.inPorts.forEach(x => {if(x) getCompShareLoop(processMap.get(x.identifier), processMap, acc) })
}

export function getComposition(port, processMap){
  if(!port){return null}
  let node = processMap.get(port.identifier)
  return node.processData.process.outPorts[port.portIndex].getComp(node.processData, processMap)
}

function applyFunc(func, params){
  let count = [];
  let res = [];
  for(let i = params.length - 1; i >= 0; i--){
    if(params[i] != null){
      count.push(i);
      res = res.concat(params[i]);
    }
  }
  res.push(func);
  let apply = count.length !=  params.length ?
    `@${count.reverse().map(x => (x + 1),toString()).join('')}` :
    `#${params.length}`
  if(apply != "@") res.push(apply)
    return res;
}

//turn {process, processData} into just processData and send the process to the processData on its creation
//  populate the in and out ports with empty arrays on creation
//  make getters and setters for the process to avoid the modification of the original process

export class ProcessData{
  constructor(process, identifier, inPorts = [], outPorts = null, data = []){
    this.process = process
    this.identifier = identifier
    this.inPorts = inPorts ? inPorts : this.inPorts = Array(process.inPorts.length).fill(null)
    this.outPorts = outPorts ? outPorts : this.outPorts = Array(process.outPorts.length).fill([])
    this.data = data
  }
  // -- only call input port functions and let them take care of the output process data ports handling
  connectToInputPort(inPortIndex, outProcessData, outPortIndex){
    //if there's something connected replace by the new connection
    if(this.inPorts[inPortIndex]){
      this.removeFromInputPort(outProcessData, inPortIndex)
    }
    
    this.inPorts[inPortIndex] = {identifier: outProcessData.identifier, portIndex: outPortIndex}
    outProcessData.outPorts[outPortIndex].push({identifier: this.identifier, portIndex: inPortIndex})
  }
  removeFromInputPort(outProcessData, inPortIndex){
    outProcessData.outPorts[this.inPorts[inPortIndex].portIndex] = 
      outProcessData.outPorts[this.inPorts[inPortIndex].portIndex].filter(x => x.identifier != this.identifier)
    this.inPorts[inPortIndex] = null;
  }
}

class Process{
  constructor(identifier, label, color, inPorts, outPorts, menu, data){
    this.identifier = identifier
    this.label = label;
    this.color = color;
    this.inPorts = inPorts;
    this.outPorts = outPorts;
    this.menu = menu;
    this.data = data
  }
}

const outputProcess = new Process(0, "output", "#B372CA", [{subtitle: "value", label: "output"}], [], [])
const inputProcess = new Process(1, "input", "#B372CA", [], [{subtitle: "string", label: "input", getComp: (pdata, pmap) => "x1"}], [])

const processList = new Map([
  [2, new Process(2,
    "more", "#000000", 
    [], 
    [ { subtitle: "", label: "value", 
      getComp: (pdata, pmap) => [`${pdata.data[0]}`] } ],
    [ { label: "value", type: "txtbox" } ],
    [""]
  )],
  [3, new Process(3,
    "constant", "#FFC107", 
    [], 
    [ { subtitle: "", label: "value", 
      getComp: (pdata, pmap) => [`${["i", "f", "s"][pdata.data[1]]}${pdata.data[0]}`] } ],
    [ { label: "value", type: "txtbox" }, 
      { label: "type",  type: "radio", 
        info: [ {label: "integer"}, {label: "float"}, {label: "string"}]
      }
    ],
    ["", 2]
  )],
  [4, new Process(4,
    "input", "#B372CA", 
    [], 
    [ { subtitle: "", label: "output", 
      getComp: (pdata, pmap) => [`x${pdata.data[0]}`] } ],
    [ { label: "input index", type: "txtbox" } ],
    ["1"]
  )],
  [5, new Process(5,
    "composition", "#B372CA", 
    [ { subtitle: "value", label: "composition"} ], 
    [ { subtitle: "", label: "function", 
      getComp: (pdata, pmap) => ["<", ...getComposition(pdata.inPorts[0], pmap), ">"] } ],
    [],
    []
  )],
  [6, new Process(6,
    "to integer", "#4A90E2", 
    [ { subtitle: "string", label: "input"} ], 
    [ { subtitle: "integer", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$toInt", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    []
  )],
  [7, new Process(7,
    "to float", "#4A90E2", 
    [ { subtitle: "string", label: "input"} ], 
    [ { subtitle: "float", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$toFloat", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    []
  )],
  [8, new Process(8,
    "to string", "#4A90E2", 
    [ { subtitle: "value", label: "input"} ], 
    [ { subtitle: "string", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$toString", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    []
  )],
  [9, new Process(9,
    "degrees to radians", "#4A90E2", 
    [ { subtitle: "number", label: "input"} ], 
    [ { subtitle: "numbet", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$degreesToRadians", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    []
  )],
  [10, new Process(10,
    "radians to degrees", "#4A90E2", 
    [ { subtitle: "number", label: "input"} ], 
    [ { subtitle: "numbet", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$radiasToDegrees", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    []
  )],
  [11, new Process(11,
    "concat", "#F25C54", 
    [ {subtitle: "string", label: "str1"}, {subtitle: "string", label: "str2"}],
    [ {subtitle: "string", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$concat", [getComposition(pdata.inPorts[0], pmap), getComposition(pdata.inPorts[1], pmap)]) } ],
    [],
    []
  )],
  [12, new Process(12,
    "sum", "#F25C54", 
    [ {subtitle: "number", label: "value 1"}, {subtitle: "number", label: "value 2"}],
    [ {subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$sum", [getComposition(pdata.inPorts[0], pmap), getComposition(pdata.inPorts[1], pmap)]) } ],
    [],
    []
  )],
  [13, new Process(13,
    "multiply", "#F25C54", 
    [ {subtitle: "number", label: "value 1"}, {subtitle: "number", label: "value 2"}],
    [ {subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$multiply", [getComposition(pdata.inPorts[0], pmap), getComposition(pdata.inPorts[1], pmap)]) } ],
    [],
    []
  )],
  [14, new Process(14,
    "divide", "#F25C54", 
    [ {subtitle: "number", label: "value 1"}, {subtitle: "number", label: "value 2"}],
    [ {subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$divide", [getComposition(pdata.inPorts[0], pmap), getComposition(pdata.inPorts[1], pmap)]) } ],
    [],
    []
  )],
  [15, new Process(15,
    "sqrt", "#F25C54", 
    [ { subtitle: "number", label: "input"} ], 
    [ { subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$sqrt", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    []
  )],
  [16, new Process(16,
    "log e", "#F25C54", 
    [ { subtitle: "number", label: "input"} ], 
    [ { subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$log", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    []
  )],
  [17, new Process(17,
    "log 10", "#F25C54", 
    [ { subtitle: "number", label: "input"} ], 
    [ { subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$log10", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    []
  )],
  [18, new Process(18,
    "log 2", "#F25C54", 
    [ { subtitle: "number", label: "input"} ], 
    [ { subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$log2", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    []
  )],
  [19, new Process(19,
    "sin", "#F25C54", 
    [ { subtitle: "number", label: "input"} ], 
    [ { subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$sin", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    []
  )],
  [20, new Process(20,
    "asin", "#F25C54", 
    [ { subtitle: "number", label: "input"} ], 
    [ { subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$asin", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    []
  )],
  [21, new Process(21,
    "cos", "#F25C54", 
    [ { subtitle: "number", label: "input"} ], 
    [ { subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$cos", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    []
  )],
  [22, new Process(22,
    "acos", "#F25C54", 
    [ { subtitle: "number", label: "input"} ], 
    [ { subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$acos", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    []
  )],
  [23, new Process(23,
    "tan", "#F25C54", 
    [ { subtitle: "number", label: "input"} ], 
    [ { subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$tan", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    []
  )],
  [24, new Process(24,
    "atan", "#F25C54", 
    [ { subtitle: "number", label: "input"} ], 
    [ { subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$atan", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    []
  )],
  [25, new Process(25,
    "erf", "#F25C54", 
    [ { subtitle: "number", label: "input"} ], 
    [ { subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$erf", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    []
  )],
  [26, new Process(26,
    "erfc", "#F25C54", 
    [ { subtitle: "number", label: "input"} ], 
    [ { subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$erfc", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    []
  )],
  [27, new Process(27,
    "ceil", "#F25C54", 
    [ { subtitle: "number", label: "input"} ], 
    [ { subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$ceil", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    []
  )],
  [28, new Process(28,
    "floor", "#F25C54", 
    [ { subtitle: "number", label: "input"} ], 
    [ { subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$floor", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    []
  )],
  [29, new Process(29,
    "pow", "#F25C54", 
    [ { subtitle: "number", label: "base"}, { subtitle: "number", label: "exponent"} ], 
    [ { subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$pow", [getComposition(pdata.inPorts[0], pmap), getComposition(pdata.inPorts[1], pmap)]) } ],
    [],
    []
  )],
  [30, new Process(30,
    "exp", "#F25C54", 
    [ { subtitle: "number", label: "exponent"} ], 
    [ { subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$exp", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    []
  )],
  [31, new Process(31,
    "length", "#FF8C00", 
    [ { subtitle: "", label: "list"} ], 
    [ { subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$length", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    []
  )],
  [32, new Process(32,
    "push", "#FF8C00", 
    [ { subtitle: "", label: "list"}, { subtitle: "", label: "element"} ], 
    [ { subtitle: "", label: "list", 
      getComp: (pdata, pmap) => applyFunc("$push", [getComposition(pdata.inPorts[0], pmap), getComposition(pdata.inPorts[1], pmap)]) } ],
    [],
    []
  )],
  [33, new Process(33,
    "pop", "#FF8C00", 
    [ { subtitle: "", label: "list"} ], 
    [ { subtitle: "", label: "list", 
      getComp: (pdata, pmap) => applyFunc("$pop", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    []
  )],
  [34, new Process(34,
    "head", "#FF8C00", 
    [ { subtitle: "", label: "list"} ], 
    [ { subtitle: "", label: "element", 
      getComp: (pdata, pmap) => applyFunc("$head", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    []
  )],
  [35, new Process(35,
    "tail", "#FF8C00", 
    [ { subtitle: "", label: "list"} ], 
    [ { subtitle: "", label: "list", 
      getComp: (pdata, pmap) => applyFunc("$tail", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    []
  )],
  [36, new Process(36,
    "list of", "#FF8C00", 
    [ { subtitle: "integer", label: "length"}, { subtitle: "", label: "element"}], 
    [ { subtitle: "", label: "list", 
      getComp: (pdata, pmap) => applyFunc("$duplicate", [getComposition(pdata.inPorts[0], pmap), getComposition(pdata.inPorts[1], pmap)]) } ],
    [],
    []
  )],
  [37, new Process(37,
    "foldl", "#FF8C00", 
    [ { subtitle: "", label: "function"}, { subtitle: "", label: "acc 0"}, { subtitle: "", label: "list"}], 
    [ { subtitle: "", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$foldl", [getComposition(pdata.inPorts[0], pmap), getComposition(pdata.inPorts[1], pmap), getComposition(pdata.inPorts[2], pmap)]) } ],
    [],
    []
  )],
  [38, new Process(38,
    "map", "#FF8C00", 
    [ { subtitle: "", label: "function"}, { subtitle: "", label: "list"}], 
    [ { subtitle: "", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$map", [getComposition(pdata.inPorts[0], pmap), getComposition(pdata.inPorts[1], pmap)]) } ],
    [],
    []
  )],
  [39, new Process(39,
    "range", "#FF8C00", 
    [ { subtitle: "integer", label: "from"}, { subtitle: "integer", label: "to"}], 
    [ { subtitle: "", label: "list", 
      getComp: (pdata, pmap) => applyFunc("$range", [getComposition(pdata.inPorts[0], pmap), getComposition(pdata.inPorts[1], pmap)]) } ],
    [],
    []
  )],
])


export function getArrowList() {
  return processList;
}

export function getOutputProcess(){
  return outputProcess;
}
export function getInputProcess(){
  return inputProcess;
}
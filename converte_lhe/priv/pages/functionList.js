export function getComposition(port, processMap){
  if(!port){return null}
  let node = processMap.get(port.identifier)
  return node.process.outPorts[port.portIndex].getComp(node.processData, processMap)
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

class Process{
  constructor(label, color, inPorts, outPorts, menu, data){
    this.label = label;
    this.color = color;
    this.inPorts = inPorts;
    this.outPorts = outPorts;
    this.menu = menu;
    this.data = data
  }
}

const processList = [
  new Process(
    "more", "#000000", 
    [], 
    [ { subtitle: "", label: "value", 
      getComp: (pdata, pmap) => [`${pdata.data[0]}`] } ],
    [ { label: "value", type: "txtbox" } ],
    [""]
  ),
  new Process(
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
  ),
  new Process(
    "input", "#B372CA", 
    [], 
    [ { subtitle: "", label: "output", 
      getComp: (pdata, pmap) => [`x${pdata.data[0]}`] } ],
    [ { label: "input index", type: "txtbox" } ],
    ["1"]
  ),
  new Process(
    "composition", "#B372CA", 
    [ { subtitle: "value", label: "composition"} ], 
    [ { subtitle: "", label: "function", 
      getComp: (pdata, pmap) => ["<", ...getComposition(pdata.inPorts[0], pmap), ">"] } ],
    [],
    []
  ),
  new Process(
    "to integer", "#4A90E2", 
    [ { subtitle: "string", label: "input"} ], 
    [ { subtitle: "integer", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$toInt", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    []
  ),
  new Process(
    "to float", "#4A90E2", 
    [ { subtitle: "string", label: "input"} ], 
    [ { subtitle: "float", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$toFloat", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    []
  ),
  new Process(
    "to string", "#4A90E2", 
    [ { subtitle: "value", label: "input"} ], 
    [ { subtitle: "string", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$toString", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    []
  ),
  new Process(
    "degrees to radians", "#4A90E2", 
    [ { subtitle: "number", label: "input"} ], 
    [ { subtitle: "numbet", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$degreesToRadians", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    []
  ),
  new Process(
    "radians to degrees", "#4A90E2", 
    [ { subtitle: "number", label: "input"} ], 
    [ { subtitle: "numbet", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$radiasToDegrees", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    []
  ),
  new Process(
    "concat", "#F25C54", 
    [ {subtitle: "string", label: "str1"}, {subtitle: "string", label: "str2"}],
    [ {subtitle: "string", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$concat", [getComposition(pdata.inPorts[0], pmap), getComposition(pdata.inPorts[1], pmap)]) } ],
    [],
    []
  ),
  new Process(
    "sum", "#F25C54", 
    [ {subtitle: "number", label: "value 1"}, {subtitle: "number", label: "value 2"}],
    [ {subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$sum", [getComposition(pdata.inPorts[0], pmap), getComposition(pdata.inPorts[1], pmap)]) } ],
    [],
    []
  ),
  new Process(
    "multiply", "#F25C54", 
    [ {subtitle: "number", label: "value 1"}, {subtitle: "number", label: "value 2"}],
    [ {subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$multiply", [getComposition(pdata.inPorts[0], pmap), getComposition(pdata.inPorts[1], pmap)]) } ],
    [],
    []
  ),
  new Process(
    "divide", "#F25C54", 
    [ {subtitle: "number", label: "value 1"}, {subtitle: "number", label: "value 2"}],
    [ {subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$divide", [getComposition(pdata.inPorts[0], pmap), getComposition(pdata.inPorts[1], pmap)]) } ],
    [],
    []
  ),
  new Process(
    "sqrt", "#F25C54", 
    [ { subtitle: "number", label: "input"} ], 
    [ { subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$sqrt", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    []
  ),
  new Process(
    "log e", "#F25C54", 
    [ { subtitle: "number", label: "input"} ], 
    [ { subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$log", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    []
  ),
  new Process(
    "log 10", "#F25C54", 
    [ { subtitle: "number", label: "input"} ], 
    [ { subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$log10", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    []
  ),
  new Process(
    "log 2", "#F25C54", 
    [ { subtitle: "number", label: "input"} ], 
    [ { subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$log2", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    []
  ),
  new Process(
    "sin", "#F25C54", 
    [ { subtitle: "number", label: "input"} ], 
    [ { subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$sin", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    []
  ),
  new Process(
    "asin", "#F25C54", 
    [ { subtitle: "number", label: "input"} ], 
    [ { subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$asin", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    []
  ),
  new Process(
    "cos", "#F25C54", 
    [ { subtitle: "number", label: "input"} ], 
    [ { subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$cos", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    []
  ),
  new Process(
    "acos", "#F25C54", 
    [ { subtitle: "number", label: "input"} ], 
    [ { subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$acos", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    []
  ),
  new Process(
    "tan", "#F25C54", 
    [ { subtitle: "number", label: "input"} ], 
    [ { subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$tan", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    []
  ),
  new Process(
    "atan", "#F25C54", 
    [ { subtitle: "number", label: "input"} ], 
    [ { subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$atan", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    []
  ),
  new Process(
    "erf", "#F25C54", 
    [ { subtitle: "number", label: "input"} ], 
    [ { subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$erf", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    []
  ),
  new Process(
    "erfc", "#F25C54", 
    [ { subtitle: "number", label: "input"} ], 
    [ { subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$erfc", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    []
  ),
  new Process(
    "ceil", "#F25C54", 
    [ { subtitle: "number", label: "input"} ], 
    [ { subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$ceil", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    []
  ),
  new Process(
    "floor", "#F25C54", 
    [ { subtitle: "number", label: "input"} ], 
    [ { subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$floor", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    []
  ),
  new Process(
    "pow", "#F25C54", 
    [ { subtitle: "number", label: "base"}, { subtitle: "number", label: "exponent"} ], 
    [ { subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$pow", [getComposition(pdata.inPorts[0], pmap), getComposition(pdata.inPorts[1], pmap)]) } ],
    [],
    []
  ),
  new Process(
    "exp", "#F25C54", 
    [ { subtitle: "number", label: "exponent"} ], 
    [ { subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$exp", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    []
  ),
  new Process(
    "length", "#FF8C00", 
    [ { subtitle: "", label: "list"} ], 
    [ { subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$length", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    []
  ),
  new Process(
    "push", "#FF8C00", 
    [ { subtitle: "", label: "list"}, { subtitle: "", label: "element"} ], 
    [ { subtitle: "", label: "list", 
      getComp: (pdata, pmap) => applyFunc("$push", [getComposition(pdata.inPorts[0], pmap), getComposition(pdata.inPorts[1], pmap)]) } ],
    [],
    []
  ),
  new Process(
    "pop", "#FF8C00", 
    [ { subtitle: "", label: "list"} ], 
    [ { subtitle: "", label: "list", 
      getComp: (pdata, pmap) => applyFunc("$pop", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    []
  ),
  new Process(
    "head", "#FF8C00", 
    [ { subtitle: "", label: "list"} ], 
    [ { subtitle: "", label: "element", 
      getComp: (pdata, pmap) => applyFunc("$head", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    []
  ),
  new Process(
    "tail", "#FF8C00", 
    [ { subtitle: "", label: "list"} ], 
    [ { subtitle: "", label: "list", 
      getComp: (pdata, pmap) => applyFunc("$tail", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    []
  ),
  new Process(
    "list of", "#FF8C00", 
    [ { subtitle: "integer", label: "length"}, { subtitle: "", label: "element"}], 
    [ { subtitle: "", label: "list", 
      getComp: (pdata, pmap) => applyFunc("$duplicate", [getComposition(pdata.inPorts[0], pmap), getComposition(pdata.inPorts[1], pmap)]) } ],
    [],
    []
  ),
  new Process(
    "foldl", "#FF8C00", 
    [ { subtitle: "", label: "function"}, { subtitle: "", label: "acc 0"}, { subtitle: "", label: "list"}], 
    [ { subtitle: "", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$foldl", [getComposition(pdata.inPorts[0], pmap), getComposition(pdata.inPorts[1], pmap), getComposition(pdata.inPorts[2], pmap)]) } ],
    [],
    []
  ),
  new Process(
    "map", "#FF8C00", 
    [ { subtitle: "", label: "function"}, { subtitle: "", label: "list"}], 
    [ { subtitle: "", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$map", [getComposition(pdata.inPorts[0], pmap), getComposition(pdata.inPorts[1], pmap)]) } ],
    [],
    []
  ),
  new Process(
    "range", "#FF8C00", 
    [ { subtitle: "integer", label: "from"}, { subtitle: "integer", label: "to"}], 
    [ { subtitle: "", label: "list", 
      getComp: (pdata, pmap) => applyFunc("$range", [getComposition(pdata.inPorts[0], pmap), getComposition(pdata.inPorts[1], pmap)]) } ],
    [],
    []
  ),
]


export function getArrowList() {
  return processList;
}

const outputProcess = new Process("output", "#B372CA", [{subtitle: "value", label: "output"}], [], [])
const inputProcess = new Process("input", "#B372CA", [], [{subtitle: "string", label: "input", getComp: (pdata, pmap) => "x1"}], [])

export function getOutputProcess(){
  return outputProcess;
}
export function getInputProcess(){
  return inputProcess;
}
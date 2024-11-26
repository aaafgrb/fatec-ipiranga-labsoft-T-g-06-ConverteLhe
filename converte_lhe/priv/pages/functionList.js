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
  connectToInputPort(inPortIndex, processRows, newOutProcessData, outPortIndex){
    //if there's something connected replace by the new connection
    if(this.inPorts[inPortIndex]){
      this.removeFromInputPort(processRows.get(this.inPorts[inPortIndex].identifier).processData, inPortIndex)
    }
    
    this.inPorts[inPortIndex] = {identifier: newOutProcessData.identifier, portIndex: outPortIndex}
    newOutProcessData.outPorts[outPortIndex].push({identifier: this.identifier, portIndex: inPortIndex})
  }
  removeFromInputPort(outProcessData, inPortIndex){
    outProcessData.outPorts[this.inPorts[inPortIndex].portIndex] = 
      outProcessData.outPorts[this.inPorts[inPortIndex].portIndex].filter(x => x.identifier != this.identifier)
    this.inPorts[inPortIndex] = null;
  }
}


const processTypesColors = new Map([
  ["special", "#000000"],
  ["composition", "#B372CA"],
  ["general", "#FFC107"],
  ["conversion", "#4A90E2"],
  ["number", "#F25C54"],
  ["list", "#FF8C00"],
  ["function", "#d1459e"],
  ["string", "#2dc47b"],
])


class Process{
  constructor(identifier, label, type, inPorts, outPorts, menu, data, doc){
    this.identifier = identifier
    this.label = label;
    this.type = type;
    this.color = processTypesColors.get(type);
    this.inPorts = inPorts;
    this.outPorts = outPorts;
    this.menu = menu;
    this.data = data;
    this.doc = doc
  }
}

const outputProcess = new Process(0, 
  "output", "composition", 
  [{subtitle: "value", label: "output"}], 
  [], 
  [], 
  [],
  {
    desc: "The output process. The values return after passing by a format function and because of the way that erlang (the server backend language) handles strings, if the retun value is an array of integers and their values are within the range of the printable ascii, it might be interpreted as an string instead of an array. And if the character binary is outsite the printable ascii (like in unicode) it might be interpreted as an array of integers",
    menu: "no",
    example: "no",
    comp: "{input 1}",
  })

  const inputProcess = new Process(1, 
  "input", "composition", 
  [], 
  [{subtitle: "string", label: "input", getComp: (pdata, pmap) => "x1"}], 
  [], 
  [], 
  {
    desc: "The input process. Is the input of the composition and always a string. Its equivalent to the placable input process with the parameter '1' (i.e. its equivalent to the first (and only) parameter of the composition)",
    menu: "no",
    example: "no",
    comp: "x1",
  })

const processList = new Map([
  [2, new Process(2,
    "more", "special", 
    [], 
    [ { subtitle: "", label: "value", 
      getComp: (pdata, pmap) => [`${pdata.data[0]}`] } ],
    [ { label: "value", type: "txtbox" } ],
    [""],
    {
      desc: "Inserts the menu value as it is to the composition",
      menu: "value (string)",
      example: "menu: \"i4/i5/$sum/#2\" | output: \"i4/i5/$sum/#2\" ",
      comp: "{menu value}",
    }
  )],
  [3, new Process(3,
    "constant", "general", 
    [], 
    [ { subtitle: "", label: "value", 
      getComp: (pdata, pmap) => [`${["i", "f", "s"][pdata.data[1]]}${pdata.data[0]}`] } ],
    [ { label: "value", type: "txtbox" }, 
      { label: "type",  type: "radio", 
        info: [ {label: "integer"}, {label: "float"}, {label: "string"}]
      }
    ],
    ["", 2],
    {
      desc: "Has a menu with a textbox and a radio list with the type that the input of the textbox will be interpreted as. dont forget to escape the '/' character with a preceding '\\'",
      menu: "Value (string) | Type [integer, float, string] (radio)",
      example: "menu: \"3\" integer | output: \"i3\"",
      comp: "{s or f or i depending on menu type}{menu value}",
    }
  )],
  //-----------------------COMPOSITION------------------------------------------
  [4, new Process(4,
    "input", "composition", 
    [], 
    [ { subtitle: "", label: "output", 
      getComp: (pdata, pmap) => [`x${pdata.data[0]}`] } ],
    [ { label: "input index", type: "txtbox" } ],
    ["1"],
    {
      desc: "Is the input of a composition. index starts on 1",
      menu: "input index (integer)",
      example: "menu: \"3\" | output: \"x3\"",
      comp: "x{menu input index}",
    }
  )],
  [5, new Process(5,
    "composition", "composition", 
    [ { subtitle: "value", label: "composition"} ], 
    [ { subtitle: "", label: "function", 
      getComp: (pdata, pmap) => ["<", ...getComposition(pdata.inPorts[0], pmap), ">"] } ],
    [],
    [],
    {
      desc: "Receives a composition and returns a function that runs the composition. Beware that its not possible to use the outer context input (the main input) on the composition, it is necessary to pass it as an parameter",
      menu: "no",
      example: "input: \"{input(1), input(1)} -> sum\" | output: \"a function that receives one value and returns the sum of it with itself\"",
      comp: "{input 1}",
    }
  )],
  [40, new Process(40,
    "apply", "composition", 
    [ { subtitle: "", label: "function"}, { subtitle: "", label: "parameter"} ], 
    [ { subtitle: "", label: "output", 
      getComp: (pdata, pmap) => [...getComposition(pdata.inPorts[1], pmap), ...getComposition(pdata.inPorts[0], pmap), `@${pdata.data[0]}`] } ],
    [ { label: "input index", type: "txtbox" } ],
    ["1"],
    {
      desc: "Has a menu that receives an integer that represents the index (1 based) that the 'parameter' will be applied to, on the 'function'",
      menu: "input index (integer)",
      example: "menu: \"2\" | input: \"{divide, constant(integer 2)}\" | output: \"a function that receives a value and divides it by 2\"",
      comp: "{input 2}/{input 1}/@{menu input index}",
    }
  )],
  //-----------------------CONVERSION------------------------------------------
  [6, new Process(6,
    "to integer", "conversion", 
    [ { subtitle: "string", label: "input"} ], 
    [ { subtitle: "integer", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$toInt", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    [],
    {
      desc: "Converts the string to integer",
      menu: "no",
      example: "input: \"string 3\" | output: \"integer 3\"",
      comp: "{input 1}/$toInt",
    }
  )],
  [7, new Process(7,
    "to float", "conversion", 
    [ { subtitle: "string", label: "input"} ], 
    [ { subtitle: "float", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$toFloat", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    [],
    {
      desc: "Converts the string to float, beware that you need to put the fractional part of the number on the string aswell (even if its 0) else it will return an error",
      menu: "no",
      example: "input: \"string 3\" | output: \"float 3\"",
      comp: "{input 1}/$toFloat",
    }
  )],
  [8, new Process(8,
    "to string", "conversion", 
    [ { subtitle: "value", label: "input"} ], 
    [ { subtitle: "string", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$toString", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    [],
    {
      desc: "Converts the input to string, it is subject to the same 'missconvertions' stated on the the output process",
      menu: "no",
      example: "input: \"integer 3\" | output: \"string 3\"",
      comp: "{input 1}/$toString",
    }
  )],
  [9, new Process(9,
    "degrees to radians", "conversion", 
    [ { subtitle: "number", label: "input"} ], 
    [ { subtitle: "numbet", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$degreesToRadians", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    [],
    {
      desc: "Converts degrees to radians",
      menu: "no",
      example: "input: \"30\" | output: \"0.523599\"",
      comp: "{input 1}/$degreesToRadians",
    }
  )],
  [10, new Process(10,
    "radians to degrees", "conversion", 
    [ { subtitle: "number", label: "input"} ], 
    [ { subtitle: "numbet", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$radiansToDegrees", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    [],
    {
      desc: "Converts radians to degrees",
      menu: "no",
      example: "input: \"1.0472\" | output: \"60\"",
      comp: "{input 1}/$radiansToDegrees",
    }
  )],
  //-----------------------STRING------------------------------------------
  [41, new Process(41,
    "split", "string", 
    [ { subtitle: "", label: "string"}, { subtitle: "string", label: "delimiter"} ], 
    [ { subtitle: "", label: "list", 
      getComp: (pdata, pmap) => applyFunc("$split", [getComposition(pdata.inPorts[0], pmap), getComposition(pdata.inPorts[1], pmap)]) } ],
    [],
    [],
    {
      desc: "Splits the string by a delimiter (doenst work with lists)",
      menu: "no",
      example: "input: \"a.a.s.d\" | output: \"[a,a,s,d]\"",
      comp: "{input 2}/{input 1}/$split",
    }
  )],
  [44, new Process(44,
    "concat", "string", 
    [ { subtitle: "", label: "string"}, { subtitle: "", label: "string"}], 
    [ { subtitle: "", label: "string", 
      getComp: (pdata, pmap) => applyFunc("$concat", [getComposition(pdata.inPorts[0], pmap), getComposition(pdata.inPorts[1], pmap)]) } ],
    [],
    [],
    {
      desc: "concatenates two strings",
      menu: "no",
      example: "input: \"{abc, 123}\" | output: \"abc123\"",
      comp: "{input 2}/{input 1}/$concat",
    }
  )],
  [46, new Process(46,
    "replace", "string", 
    [ { subtitle: "string", label: "subject"}, { subtitle: "string", label: "replacement"}], 
    [ { subtitle: "", label: "string", 
      getComp: (pdata, pmap) => applyFunc("$replace", [getComposition(pdata.inPorts[0], pmap), `s${pdata.data[0]}`, getComposition(pdata.inPorts[1], pmap)]) } ],
    [ { label: "match", type: "txtbox" } ],
    ["$"],
    {
      desc: "replaces a pattern on a string",
      menu: "Match (string)",
      example: "menu: \"$\" | input: \"{a$bc$, 123}\" | output: \"a123bc123\"",
      comp: "{input 2}/{menu match}/{input 1}/$replace",
    }
  )],
  //-----------------------NUMBER------------------------------------------
  [12, new Process(12,
    "sum", "number", 
    [ {subtitle: "number", label: "value 1"}, {subtitle: "number", label: "value 2"}],
    [ {subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$sum", [getComposition(pdata.inPorts[0], pmap), getComposition(pdata.inPorts[1], pmap)]) } ],
    [],
    [],
    {
      desc: "Sums",
      menu: "no",
      example: "input: \"{5, 4.2}\" | output: \"9.2\"",
      comp: "{input 2}/{input 1}/$sum",
    }
  )],
  [43, new Process(43,
    "subtract", "number", 
    [ {subtitle: "number", label: "value 1"}, {subtitle: "number", label: "value 2"}],
    [ {subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$subtract", [getComposition(pdata.inPorts[0], pmap), getComposition(pdata.inPorts[1], pmap)]) } ],
    [],
    [],
    {
      desc: "Subtracts value 2 from value 1",
      menu: "no",
      example: "input: \"{5, 4.2}\" | output: \"0.8\"",
      comp: "{input 2}/{input 1}/$subtract",
    }
  )],
  [13, new Process(13,
    "multiply", "number", 
    [ {subtitle: "number", label: "value 1"}, {subtitle: "number", label: "value 2"}],
    [ {subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$multiply", [getComposition(pdata.inPorts[0], pmap), getComposition(pdata.inPorts[1], pmap)]) } ],
    [],
    [],
    {
      desc: "Multiplies",
      menu: "no",
      example: "input: \"{5, 4.2}\" | output: \"21\"",
      comp: "{input 2}/{input 1}/$multiply",
    }
  )],
  [14, new Process(14,
    "divide", "number", 
    [ {subtitle: "number", label: "value 1"}, {subtitle: "number", label: "value 2"}],
    [ {subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$divide", [getComposition(pdata.inPorts[0], pmap), getComposition(pdata.inPorts[1], pmap)]) } ],
    [],
    [],
    {
      desc: "Divides",
      menu: "no",
      example: "input: \"{5, 4.2}\" | output: \"1.19047619048\"",
      comp: "{input 2}/{input 1}/$divide",
    }
  )],
  [15, new Process(15,
    "sqrt", "number", 
    [ { subtitle: "number", label: "input"} ], 
    [ { subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$sqrt", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    [],
    {
      desc: "Square root",
      menu: "no",
      example: "input: \"25\" | output: \"5\"",
      comp: "{input 1}/$sqrt",
    }
  )],
  [16, new Process(16,
    "log e", "number", 
    [ { subtitle: "number", label: "input"} ], 
    [ { subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$log", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    [],
    {
      desc: "Log base e",
      menu: "no",
      example: "input: \"10\" | output: \"2.302585092994\"",
      comp: "{input 1}/$log",
    }
  )],
  [17, new Process(17,
    "log 10", "number", 
    [ { subtitle: "number", label: "input"} ], 
    [ { subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$log10", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    [],
    {
      desc: "Log base 10",
      menu: "no",
      example: "input: \"100\" | output: \"2\"",
      comp: "{input 1}/$log10",
    }
  )],
  [18, new Process(18,
    "log 2", "number", 
    [ { subtitle: "number", label: "input"} ], 
    [ { subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$log2", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    [],
    {
      desc: "Log base 2",
      menu: "no",
      example: "input: \"100\" | output: \"6.6438561897747\"",
      comp: "{input 1}/$log2",
    }
  )],
  [19, new Process(19,
    "sin", "number", 
    [ { subtitle: "number", label: "input"} ], 
    [ { subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$sin", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    [],
    {
      desc: "Sine function. the input is in radians",
      menu: "no",
      example: "input: \"3\" | output: \"0.14112000806\"",
      comp: "{input 1}/$sin",
    }
  )],
  [20, new Process(20,
    "asin", "number", 
    [ { subtitle: "number", label: "input"} ], 
    [ { subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$asin", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    [],
    {
      desc: "Arc sine function. the input is in radians",
      menu: "no",
      example: "input: \"1\" | output: \"1.5707963267948966\"",
      comp: "{input 1}/$asin",
    }
  )],
  [21, new Process(21,
    "cos", "number", 
    [ { subtitle: "number", label: "input"} ], 
    [ { subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$cos", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    [],
    {
      desc: "Consine function. the input is in radians",
      menu: "no",
      example: "input: \"3\" | output: \"-0.9899924966004454\"",
      comp: "{input 1}/$cos",
    }
  )],
  [22, new Process(22,
    "acos", "number", 
    [ { subtitle: "number", label: "input"} ], 
    [ { subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$acos", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    [],
    {
      desc: "Arc cosine function. the input is in radians",
      menu: "no",
      example: "input: \"0.3\" | output: \"1.2661036727794992\"",
      comp: "{input 1}/$acos",
    }
  )],
  [23, new Process(23,
    "tan", "number", 
    [ { subtitle: "number", label: "input"} ], 
    [ { subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$tan", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    [],
    {
      desc: "Tangent function. the input is in radians",
      menu: "no",
      example: "input: \"0.3\" | output: \"0.30933624960962325\"",
      comp: "{input 1}/$tan",
    }
  )],
  [24, new Process(24,
    "atan", "number", 
    [ { subtitle: "number", label: "input"} ], 
    [ { subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$atan", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    [],
    {
      desc: "Arc tangent function. the input is in radians",
      menu: "no",
      example: "input: \"0.3\" | output: \"0.2914567944778671\"",
      comp: "{input 1}/$atan",
    }
  )],
  [25, new Process(25,
    "erf", "number", 
    [ { subtitle: "number", label: "input"} ], 
    [ { subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$erf", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    [],
    {
      desc: "Error function",
      menu: "no",
      example: "input: \"0.3\" | output: \"0.3286267594591274\"",
      comp: "{input 1}/$erf",
    }
  )],
  [26, new Process(26,
    "erfc", "number", 
    [ { subtitle: "number", label: "input"} ], 
    [ { subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$erfc", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    [],
    {
      desc: "Complementary error function",
      menu: "no",
      example: "input: \"0.3\" | output: \"0.6713732405408726\"",
      comp: "{input 1}/$erfc",
    }
  )],
  [27, new Process(27,
    "ceil", "number", 
    [ { subtitle: "number", label: "input"} ], 
    [ { subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$ceil", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    [],
    {
      desc: "Ceil",
      menu: "no",
      example: "input: \"0.3\" | output: \"1.0\"",
      comp: "{input 1}/$ceil",
    }
  )],
  [28, new Process(28,
    "floor", "number", 
    [ { subtitle: "number", label: "input"} ], 
    [ { subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$floor", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    [],
    {
      desc: "Floor",
      menu: "no",
      example: "input: \"0.3\" | output: \"0.0\"",
      comp: "{input 1}/$floor",
    }
  )],
  [29, new Process(29,
    "pow", "number", 
    [ { subtitle: "number", label: "base"}, { subtitle: "number", label: "exponent"} ], 
    [ { subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$pow", [getComposition(pdata.inPorts[0], pmap), getComposition(pdata.inPorts[1], pmap)]) } ],
    [],
    [],
    {
      desc: "Power",
      menu: "no",
      example: "input: \"{0.3, 3}\" | output: \"0.026999999999999996\"",
      comp: "{input 2}/{input 1}/$pow",
    }
  )],
  [30, new Process(30,
    "exp", "number", 
    [ { subtitle: "number", label: "exponent"} ], 
    [ { subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$exp", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    [],
    {
      desc: "Exponential",
      menu: "no",
      example: "input: \"0.3\" | output: \"1.3498588075760032\"",
      comp: "{input 1}/$exp",
    }
  )],
  //-----------------------LIST------------------------------------------
  [31, new Process(31,
    "length", "list", 
    [ { subtitle: "", label: "list"} ], 
    [ { subtitle: "number", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$arr_length", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    [],
    {
      desc: "Returns the length of an array as an integer",
      menu: "no",
      example: "input: \"[3,1,3]\" | output: \"3\"",
      comp: "{input 2}/{input 1}/$arr_length",
    }
  )],
  [42, new Process(42,
    "index", "list", 
    [ { subtitle: "", label: "list"} ], 
    [ { subtitle: "", label: "element", 
      getComp: (pdata, pmap) => applyFunc("$arr_index", [getComposition(pdata.inPorts[0], pmap), [`i${pdata.data[0]}`]]) } ],
    [{ label: "index", type: "txtbox" }],
    ["1"],
    {
      desc: "Has a menu receiving a integer. returns the element of index of that integer",
      menu: "index (integer)",
      example: "menu: \"2\" | input: \"[3,1,3]\" | output: \"1\"",
      comp: "{menu index}/{input 1}/$arr_index",
    }
  )],
  [32, new Process(32,
    "push", "list", 
    [ { subtitle: "", label: "list"}, { subtitle: "", label: "element"} ], 
    [ { subtitle: "", label: "list", 
      getComp: (pdata, pmap) => applyFunc("$arr_push", [getComposition(pdata.inPorts[0], pmap), getComposition(pdata.inPorts[1], pmap)]) } ],
    [],
    [],
    {
      desc: "Pushes an value into an array",
      menu: "no",
      example: "input: \"{[1,2,34], 3}\" | output: \"[1,2,34,3]\"",
      comp: "{input 2}/{input 1}/$arr_push",
    }
  )],
  [33, new Process(33,
    "pop", "list", 
    [ { subtitle: "", label: "list"} ], 
    [ { subtitle: "", label: "list", 
      getComp: (pdata, pmap) => applyFunc("$arr_pop", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    [],
    {
      desc: "Pops the last value of an array",
      menu: "no",
      example: "input: \"[1,2,34]\" | output: \"[1,2]\"",
      comp: "{input 1}/$arr_pop",
    }
  )],
  [34, new Process(34,
    "head", "list", 
    [ { subtitle: "", label: "list"} ], 
    [ { subtitle: "", label: "element", 
      getComp: (pdata, pmap) => applyFunc("$arr_head", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    [],
    {
      desc: "Gets the head (first value) of an array",
      menu: "no",
      example: "input: \"[1,2,34]\" | output: \"1\"",
      comp: "{input 1}/$arr_head",
    }
  )],
  [35, new Process(35,
    "tail", "list", 
    [ { subtitle: "", label: "list"} ], 
    [ { subtitle: "", label: "list", 
      getComp: (pdata, pmap) => applyFunc("$arr_tail", [getComposition(pdata.inPorts[0], pmap)]) } ],
    [],
    [],
    {
      desc: "Gets the tail (all except the first value) of an array",
      menu: "no",
      example: "input: \"[1,2,34]\" | output: \"[2,34]\"",
      comp: "{input 1}/$arr_tail",
    }
  )],
  [36, new Process(36,
    "list of", "list", 
    [ { subtitle: "integer", label: "length"}, { subtitle: "", label: "element"}], 
    [ { subtitle: "", label: "list", 
      getComp: (pdata, pmap) => applyFunc("$arr_duplicate", [getComposition(pdata.inPorts[0], pmap), getComposition(pdata.inPorts[1], pmap)]) } ],
    [],
    [],
    {
      desc: "Makes an array with copies of the same value",
      menu: "no",
      example: "input: \"{4, 3}\" | output: \"[3,3,3,3]\"",
      comp: "{input 2}/{input 1}/$arr_duplicate",
    }
  )],
  [45, new Process(45,
    "intersperse", "list", 
    [ { subtitle: "", label: "list"}, { subtitle: "", label: "delimiter"}], 
    [ { subtitle: "", label: "list", 
      getComp: (pdata, pmap) => applyFunc("$arr_intersperse", [getComposition(pdata.inPorts[0], pmap), getComposition(pdata.inPorts[1], pmap)]) } ],
    [],
    [],
    {
      desc: "intersperse a value on a list",
      menu: "no",
      example: "input: \"{[1,2,34], 3}\" | output: \"[1,3,2,3,34]\"",
      comp: "{input 2}/{input 1}/$arr_intersperse",
    }
  )],
  [39, new Process(39,
    "range", "list", 
    [ { subtitle: "integer", label: "from"}, { subtitle: "integer", label: "to"}], 
    [ { subtitle: "", label: "list", 
      getComp: (pdata, pmap) => applyFunc("$arr_range", [getComposition(pdata.inPorts[0], pmap), getComposition(pdata.inPorts[1], pmap)]) } ],
    [],
    [],
    {
      desc: "Makes an array containing the sequence of numbers from and to the input integers",
      menu: "no",
      example: "input: \"{1, 3}\" | output: \"[1,2,3]\"",
      comp: "{input 2}/{input 1}/$arr_range",
    }
  )],
  [11, new Process(11,
    "concat", "list", 
    [ {subtitle: "string", label: "str1"}, {subtitle: "string", label: "str2"}],
    [ {subtitle: "string", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$arr_concat", [getComposition(pdata.inPorts[0], pmap), getComposition(pdata.inPorts[1], pmap)]) } ],
    [],
    [],
    {
      desc: "Concatenates lists",
      menu: "no",
      example: "input: \"{[3,1,2], [a,d,f]}\" | output: \"[3,1,2,a,d,f]\"",
      comp: "{input 2}/{input 1}/$arr_concat",
    }
  )],
  //-----------------------FUNCTION------------------------------------------
  [37, new Process(37,
    "foldl", "function", 
    [ { subtitle: "", label: "function"}, { subtitle: "", label: "acc 0"}, { subtitle: "", label: "list"}], 
    [ { subtitle: "", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$foldl", [getComposition(pdata.inPorts[0], pmap), getComposition(pdata.inPorts[1], pmap), getComposition(pdata.inPorts[2], pmap)]) } ],
    [],
    [],
    {
      desc: "Fold left (aka reduce) an array",
      menu: "no",
      example: "input: \"{$divide, 3, [2,3]}\" | output: \"4.5\"",
      comp: "{input 3}/{input 2}/{input 1}/$foldl",
    }
  )],
  [38, new Process(38,
    "map", "function", 
    [ { subtitle: "", label: "function"}, { subtitle: "", label: "list"}], 
    [ { subtitle: "", label: "output", 
      getComp: (pdata, pmap) => applyFunc("$map", [getComposition(pdata.inPorts[0], pmap), getComposition(pdata.inPorts[1], pmap)]) } ],
    [],
    [],
    {
      desc: "Applies a function to every element of an array",
      menu: "no",
      example: "input: \"{$sum(4), [2,3]}\" | output: \"[6,7]\"",
      comp: "{input 2}/{input 1}/$map",
    }
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
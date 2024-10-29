import { getArrowList, getOutputProcess, getInputProcess} from "./functionList.js"

Vue.createApp({
  components: {
  },
  data: () => ({
    processList: []
  }),
  beforeMount(){
    let al = getArrowList();
    let o = getOutputProcess();
    let i = getInputProcess();
    this.processList = new Map([[o.identifier, o], [i.identifier, i], ...al])
  },
  methods: {
  },
  template: `
  <h2 style="color: white">System</h2>
  <h4 style="color: white; margin-left: 15px">Composing</h4>
    <p style="color: white; margin-left: 15px">
    The composition is created by dragging 'processes' from the process area (bottom right) to the composition area (center top), snapping them to the points and connecting them by dragging connections from the input to the output 'port' (or vice versa). 
    A connection cant be made going from and to the same node and all input ports (left side of the process node) can only make one connection (but you can make as many as you want from a output port). <br>
    When running or making the share url the composition is read from the output process, therefore all nodes that dont connect to any nodes connected to the output port will be ignored. <br>
    </p>
  <h4 style="color: white; margin-left: 15px">Composition</h4>
    <p style="color: white; margin-left: 15px">
    Each process node represent one or more parts of the composition string and they are put together based on the connections of the nodes. <br>
    The composition string is formatted like: '{prefix}{value}/{prefix}{value}/{prefix}{value}' using the rpn (reverse polish notation) format. Note that the '/' is part of the syntax so if you need to use it as a literal you should escape it by preceding it with a '\\'. <br>
    The prefixes tell the system how the value will be read and are a single character, they are: i, f, s, x, $, #, @, < and >.
    <br>- i: integer
    <br>- f: float (it is required to have the decimal part of the value, even if it is 0)
    <br>- s: string 
    <br>- x: parameter (refers to the input parameter index (1 based) of the current composition, if its inside an inner composition it refers to the input parameter index of that instead)
    <br>- $: function 
    <br>- #: apply all (applies the value number of parameters to the preceding function)
    <br>- @: apply to index (applies the value two steps behind to the function one step behind at the index of the value (1 based))
    <br>- <: start composition (the value doesnt matter, refers to the start of an inner composition, requires an end composition to close it)
    <br>- >: end composition (the value doesnt matter, closes the opened composition. if there isnt an open composition it is ignored)
    </p>
  <h5 style="color: white; margin-left: 15px">Examples</h5>
    <details style="color: white; margin-left: 15px">
      <summary>s123/s456/$concat/#2</summary>
          | push the string 123 into the stack
      <br>| push the string 456 into the stack
      <br>| push the function 'concat' into the stack
      <br>| applies the values 2 and 3 steps behind ('456' and '123') to the value behind (function 'concat')
      <br>| results in '456123'
    </details>
    <details style="color: white; margin-left: 15px">
      <summary>s123/s456/$concat/@2/@1</summary>
          | push the string 123 into the stack
      <br>| push the string 456 into the stack
      <br>| push the function 'concat' into the stack
      <br>| applies the value 2 steps behind (string '456') to the value behind (function 'concat')
      <br>| applies the value 2 steps behind (string '123') to the value behind (function 'concat')
      <br>| results in '123456'
    </details>
    <details style="color: white; margin-left: 15px">
      <summary>x1/$toInt/#1/i3/$sum/#2</summary>
          | push the value of the composition input to the stack
      <br>| push the function 'toInt' to the stack
      <br>| applies the value 2 steps behind (value of the input of the composition) to the value behind (function 'toint')
      <br>| push the integer '3; to the stack
      <br>| push the function 'sum' to the stack
      <br>| applies the values 2 and 3 steps behind ('3' and value of the input of the composition) to the value behind (function 'sum')
      <br>| results in 3 + (the input value)
    </details>
    <details style="color: white; margin-left: 15px">
      <summary>s4/x1/$concat/#2/&lt/x1/s3/$concat/#2/&gt/#1</summary>
          | push the string '4' to the stack
      <br>| push the value of the composition input to the stack
      <br>| push the function 'concat' to the stack
      <br>| applies the values 2 and 3 steps behind (value of the input of the composition and '123') to the value behind (function 'concat')
      <br>| start reading inner composition
      <br>| | push the value of the input of the composition to the stack
      <br>| | push the string '3' to the stack
      <br>| | push the function 'concat' to the stack
      <br>| | push the apply all '2' to the stack
      <br>| push the function of the inner composition to the stack
      <br>| applies the value 2 steps behind (the result of s4/x1/$concat/#2/) to the value behind (function of the composition x1/s3/$concat/#2/)
      <br>| results in '3' ++ (the input value) ++ '4'
    </details>
  <h4 style="color: white; margin-left: 15px">Share URL</h4>
    <p style="color: white; margin-left: 15px">
    The share url is generated by travessing the composition the same way as the composition getting, but each node generates a string with the format: {node id}/{process id}/{process parameters}/{connections} <br>
    - node id is an integer representing a node<br>
    - process id is an integer that represents the process of the node<br>
    - process parameters is a list of strings (splitted by commas) representing the data of the menu<br>
    - connections is a list of the format {process id}:{port index} splitted by commas representing the connections that each input port that the current node makes<br>
    and each of these strings are splitted by a '|', therefore the special characters , : / and | need to be escaped when used as menu parameters (this is done automatically)<br>
    </p>
  <hr style="color: white;">
  <h2 style="color: white">Process List</h2>
  <ul>
    <li style="color: white" v-for="[k, p] in processList">
      <a class="row" :href="\`#\${p.identifier}\`">.{{p.label}}</a>
    </li>
  </ul>
  <hr style="color: white;">
  <div v-for="[k, p] in processList">
    <div class="container mt-4">
      <div class="row">
        <div :style="\`border: 1px solid #000000; border-radius: 15px; background-color: \${p.color} \`">
          <h4 :id="p.identifier" style="color: white">{{p.label}}</h4>
          <p class="text-left" style="color: white">{{p.doc}}</p>
        </div>
      </div>
    </div>
  </div>
  `
}).mount('#app')



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
    <br>- x: parameter
    <br>- $: function
    <br>- #: apply all
    <br>- @: apply to index
    <br>- <: start composition
    <br>- >: end composition

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



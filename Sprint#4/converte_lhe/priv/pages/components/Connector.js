export default {
  components: {
  },
  props: {
    inPort: {
      type: Object,
      required: true
    },
    outPort: {
      type: Object,
      required: true
    },
    index: {
      type: Number,
      required: false
    }
  },
  watch: {
    'inPort.position': {
      handler(){
        this.updatePath();
      },
      deep: true
    },
    'outPort.position': {
      handler(){
        this.updatePath();
      },
      deep: true
    }
  },
  computed: {
  },
  expose: ['updatePath'],
  data: () => ({
    data: "",
  }),
  methods: {
    updatePath() {
      const r1 = this.inPort.position;
      const r4 = this.outPort.position;
      const x1 = r1.x;
      const y1 = r1.y;
      const x4 = r4.x;
      const y4 = r4.y;

      const bezierWeight = 0.675;

      const dx = Math.abs(x1 - x4) * bezierWeight;
  
      const p1x = x1;
      const p1y = y1;
  
      const p2x = x1 - dx;
      const p2y = y1;
  
      const p4x = x4;
      const p4y = y4;
  
      const p3x = x4 + dx;
      const p3y = y4;
  
      this.data = `M${p1x} ${p1y} C ${p2x} ${p2y} ${p3x} ${p3y} ${p4x} ${p4y}`;
    },
  },
  mounted(){
    this.inPort.connector = this
    this.updatePath()
  },
  template: `
  <g class="connector">
    <path class="connector-path-outline" :d="data" />
    <path class="connector-path" :d="data" />
    <circle class="connector-handle input-handle" ref="inputHandle" cx="0" cy="0" r="4" 
      :transform="\`translate(\${inPort.position.x}, \${inPort.position.y})\`"
      />
    <circle class="connector-handle output-handle" ref="outputHandle" cx="0" cy="0" r="4" 
      :transform="\`translate(\${outPort.position.x}, \${outPort.position.y})\`"
      />
  </g>
  `
}


export default {
  components: {
  },
  expose: ['getspecialRowPoints'],
  props: {
    cols: { 
      type: Object,
      required: true
    },
    nullProcessRows: {
      type: Object,
      requred: true
    },
    processRows: {
      type: Object,
      requred: true
    }
  },
  mounted(){
    this.getPoints();
  },
  updated(){
    this.getPoints();
  },
  data: () => ({
  }),
  methods:{
    getPoints(){
      // this.processRows.length = 0;
      this.nullProcessRows.length = 0;
      this.cols.forEach((col, cid) => {
        let gcol = this.$refs.grid[cid]
        col.rows.forEach((row, rid) => {
          let grow = gcol.children[rid];
          let point = grow.children[0];
          row.point = point;
          if(grow.classList.contains('trow')){this.nullProcessRows.push(row)}
          // if(grow.classList.contains('crow')){ this.processRows[row.identifier] = row; }
        })
      })
    },
    getspecialRowPoints(){
      return {input: this.$refs.inputRow, output: this.$refs.outputRow}
    }
  },

  template: `
  <div class="row tarea gx-0">
    <div class="col scol"><div class="row srow"><div class="srow-input" ref="inputRow"></div></div></div>
    <div class="col tcol" v-for="col in cols" ref="grid">
      <div class="row" :class="row.processData != null ? 'crow' : 'trow'" v-for="row in col.rows">
        <div class="trow-visual">

        </div>
      </div>
    </div>
    <div class="col scol"><div class="row srow"><div class="srow-output" ref="outputRow"></div></div></div>
  </div>
  `
}


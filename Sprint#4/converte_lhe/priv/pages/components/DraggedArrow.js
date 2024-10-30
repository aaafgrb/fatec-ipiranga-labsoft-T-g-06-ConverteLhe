export default {
  components: {
  },
  inject: ['mousePagePos', 'currentArrowData'],
  emits: ['onsnap'],
  props: {
    points: {
      type: Object,
      required: true
    }
  },
  mounted(){
    this.currentArrowData.callback = this.onArrowEndDrag;
  },
  data: () => ({
    snappedPointIndex: null,
  }),
  methods: {
    onArrowEndDrag(){
      if(this.snappedPointIndex != null){
        this.$emit('onsnap', this.snappedPointIndex);
      }
    },
    checkSnap(){
      for (let i = 0; i < this.points.length; i++){
        let rect = this.points[i].point.getBoundingClientRect();
        let dist = Math.sqrt((this.mousePagePos.x - rect.left - window.scrollX) ** 2 + (this.mousePagePos.y - rect.top - window.scrollY) ** 2);
        if (dist < 40) {
          this.snappedPointIndex = i;
          return {x: rect.left + window.scrollX - 50, y: rect.top + window.scrollY - 50}
        }
      }
      this.snappedPointIndex = null
      return {x: this.mousePagePos.x - 50, y: this.mousePagePos.y - 50};
    }
  },
  computed:{
    position(){
      return this.checkSnap();
    }
  },
  template: `
  <div class="draggable" 
    :style="{left: \`\${position.x}px\`, top: \`\${position.y}px\`, backgroundColor: \`\${currentArrowData.currentArrow.color}\`}"
  >{{currentArrowData.currentArrow.label}}</div>
  `
}
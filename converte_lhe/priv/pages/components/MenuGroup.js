export default {
  components: {
  },
  props: {
    data: {
      type: Object,
      required: true
    },
    curr: {
      type: Object,
      required: true
    },
    index: {
      type: Number,
      required: true
    }
  },
  inject: [],
  data: () => ({

  }),
  mounted(){
  },
  beforeUnmount(){
    if(this.data.type == "txtbox")
      this.curr[this.index] = this.$refs.input.value
    else if(this.data.type == "radio"){
      let selected = this.$refs.input.filter(x => x.checked)[0]
      if(selected){
        this.curr[this.index] = parseInt(selected.getAttribute('i'))
      }
    }
  },
  template: `
  <div class="menu-group" v-if="data.type == 'txtbox'">
    <label class="menu-label">{{data.label}}</label>
    <input class="menu-input" :value="curr[index]" ref="input">
  </div>
  <div class="menu-group" v-if="data.type == 'radio'">
    <label class="menu-label">{{data.label}}</label>
    <fieldset>
      <div style="display: inline-block" v-for="(r, i) in data.info">
        <input type="radio" class="menu-input" :id="\`menuLabel\${i}\`" name="menuLabel" :value="\`value\${i}\`" 
          :checked="curr[index] == i" ref="input" :i="i">
        <label :for="\`menuLabel\${i}\`">{{r.label}}</label>
      </div>
    </fieldset>
  </div>
  `
}
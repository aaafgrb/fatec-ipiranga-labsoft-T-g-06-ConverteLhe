import { getCookie } from "../Util.js"
export default {
  components: {
  },
  props: {
  },
  data(){
    return { 

    }
  },
  inject: ['getComposition'],
  mounted(){
    let ckey = getCookie("convertelhekey");
    if(ckey != ""){
      this.$refs.apikeyTxt.value = ckey;
    }

  },
  methods:{
    handleResponse(r){
      this.$refs.outTxtBox.value = r.error != "" ? r.error : r.result.join('\n')
    },
    sendRequest() {
      let file = this.$refs.formFile;
      if(file.files.length == 0){
        this.nonFileRequest().then(r => 
          r.json().then(rr => this.handleResponse(rr))
        )
      }else{
        this.fileRequest().then(r => 
          r.json().then(rr => this.handleResponse(rr))
        )
      }
    },
    nonFileRequest(){
      const inData = this.$refs.inTxtBox;
      var separateLines = inData.value.split(/\r?\n|\r|\n/g);
      return fetch("./api?comp=" + encodeURIComponent(this.getComp()), {
        method: 'POST',
        body: JSON.stringify({ processThis: separateLines })
      });
    },
    fileRequest(){
      const form = this.$refs.inForm;
      const formData  = new FormData(form);
        
      //todo: insert composition data here
      //formData.append("test", "dataT");
      formData.append("comp", this.getComp())

      return fetch("./form", {
        method: 'POST',
        body: formData
      });
    },
    userButton(){
      location.href="./user"
    },
    shareButton(){
      navigator.clipboard.writeText(window.location.host + "/share?comp=" + encodeURIComponent(this.getComp()));
    },
    getComp(){
      return this.getComposition();
    },
  },
  template: `
  <form id="inForm" ref="inForm">
    <div class="row g-0">
      <button class="btn btn-secondary col-1" type="button" onclick="userButton()">
        <svg xmlns="http://www.w3.org/2000/svg" width="25" height="25" fill="currentColor" class="bi bi-person-fill" viewBox="0 0 16 16">
          <path d="M3 14s-1 0-1-1 1-4 6-4 6 3 6 4-1 1-1 1zm5-6a3 3 0 1 0 0-6 3 3 0 0 0 0 6"></path>
        </svg>
      </button>
      <div class="input-group col">
        <input class="form-control form-control-file" type="file" name="formFile" id="formFile" ref="formFile">
        <button class="btn btn-primary col-1" type="button" id="buttonSubmit" name="buttonSubmit" :onclick="sendRequest">Go</button>
        <input class="form-control" type="password" placeholder="API key" id="apikeyTxt" ref="apikeyTxt" name="apiKey">
      </div>
      <button class="btn btn-secondary col-1" type="button" onclick="shareButton()">
        <svg xmlns="http://www.w3.org/2000/svg" width="25" height="25" fill="currentColor" class="bi bi-share" viewBox="0 0 16 16">
          <path d="M13.5 1a1.5 1.5 0 1 0 0 3 1.5 1.5 0 0 0 0-3M11 2.5a2.5 2.5 0 1 1 .603 1.628l-6.718 3.12a2.5 2.5 0 0 1 0 1.504l6.718 3.12a2.5 2.5 0 1 1-.488.876l-6.718-3.12a2.5 2.5 0 1 1 0-3.256l6.718-3.12A2.5 2.5 0 0 1 11 2.5m-8.5 4a1.5 1.5 0 1 0 0 3 1.5 1.5 0 0 0 0-3m11 5.5a1.5 1.5 0 1 0 0 3 1.5 1.5 0 0 0 0-3"/>
        </svg>
      </button>
    </div>
  </form>
  in
  <div class="input-group">
    <textarea class="form-control" style="overflow-wrap: normal; overflow-x: scroll;" id="inTxtBox" name="inTxtBox" ref="inTxtBox"></textarea>
  </div>
  out
  <div class="input-group">
    <textarea class="form-control" style="overflow-wrap: normal; overflow-x: scroll;" readonly id="outTxtBox" name="outTxtBox" ref="outTxtBox"></textarea>
  </div>
  `
}
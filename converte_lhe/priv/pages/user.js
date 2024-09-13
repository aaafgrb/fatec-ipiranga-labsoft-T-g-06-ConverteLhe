function newUser() {
  makeReq("newUser", 
    (json) => document.getElementById("feedbackLbl").innerHTML = "fetch result: " + json.value
  );
}

function login() {
  makeReq("login", 
    (json) => { 
      document.getElementById("apikeyTxt").value = json.value; 
      document.getElementById("limiteTxt").value = "unimplemented"; 
    }
  );
}

function forgotPass() {
  makeReq("newPass", 
    (json) => document.getElementById("feedbackLbl").innerHTML = "fetch result: " + json.value
  );
}

function makeReq(req, callback){
  const email = document.getElementById("emailTxt");
  const pass = document.getElementById("passTxt");
  
  const feedback = document.getElementById("feedbackLbl")

  const obj = {
      req: req,
      email: email.value,
      pass: pass.value,
  };
  
  const response = fetch("./auseronn", {
    method: 'POST',
    body: JSON.stringify(obj),
  });

  feedback.innerHTML = "fetching...";

  response.then(async (res) => {
    feedback.innerHTML = res.ok ? "fetch success" : "fetch failed";
    callback(await res.json());
  })
}
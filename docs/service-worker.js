if(!self.define){const e=e=>{"require"!==e&&(e+=".js");let r=Promise.resolve();return s[e]||(r=new Promise(async r=>{if("document"in self){const s=document.createElement("script");s.src=e,document.head.appendChild(s),s.onload=r}else importScripts(e),r()})),r.then(()=>{if(!s[e])throw new Error(`Module ${e} didn’t register its module`);return s[e]})},r=(r,s)=>{Promise.all(r.map(e)).then(e=>s(1===e.length?e[0]:e))},s={require:Promise.resolve(r)};self.define=(r,i,t)=>{s[r]||(s[r]=Promise.resolve().then(()=>{let s={};const n={uri:location.origin+r.slice(1)};return Promise.all(i.map(r=>{switch(r){case"exports":return s;case"module":return n;default:return e(r)}})).then(e=>{const r=t(...e);return s.default||(s.default=r),s})}))}}define("./service-worker.js",["./workbox-468c4d03"],(function(e){"use strict";e.skipWaiting(),e.clientsClaim(),e.precacheAndRoute([{url:"assets/ChineseQuickMapping.json",revision:"55f140b0176d1f19db0ddf1d46e9dabd"},{url:"assets/GitHub-Mark-64px.png",revision:"438c17272c5f0e9f4a6da34d3e4bc5bd"},{url:"main.bf803f09e203949f1624.js",revision:"58015e9e560c716122842bccdc1e7c18"},{url:"manifest.webmanifest",revision:"1477c5af0cffd5b50304645ddc13f24e"}],{})}));

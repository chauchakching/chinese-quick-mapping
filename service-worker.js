if(!self.define){const e=e=>{"require"!==e&&(e+=".js");let s=Promise.resolve();return r[e]||(s=new Promise((async s=>{if("document"in self){const r=document.createElement("script");r.src=e,document.head.appendChild(r),r.onload=s}else importScripts(e),s()}))),s.then((()=>{if(!r[e])throw new Error(`Module ${e} didn’t register its module`);return r[e]}))},s=(s,r)=>{Promise.all(s.map(e)).then((e=>r(1===e.length?e[0]:e)))},r={require:Promise.resolve(s)};self.define=(s,i,n)=>{r[s]||(r[s]=Promise.resolve().then((()=>{let r={};const o={uri:location.origin+s.slice(1)};return Promise.all(i.map((s=>{switch(s){case"exports":return r;case"module":return o;default:return e(s)}}))).then((e=>{const s=n(...e);return r.default||(r.default=s),r}))})))}}define("./service-worker.js",["./workbox-15dd0bab"],(function(e){"use strict";self.skipWaiting(),e.clientsClaim(),e.precacheAndRoute([{url:"assets/ChineseQuickMapping.json",revision:"55f140b0176d1f19db0ddf1d46e9dabd"},{url:"assets/ChineseQuickMappingSmall.json",revision:"a4e53c0823418878a07430578c3645d3"},{url:"assets/GitHub-Mark-64px.png",revision:"438c17272c5f0e9f4a6da34d3e4bc5bd"},{url:"assets/icon-200-color.png",revision:"166c48d2f4a2ed36412665c312103fa2"},{url:"assets/icon-200.png",revision:"0c0b51caa5be86e091cc97924629b45a"},{url:"assets/icon-512.png",revision:"b1cac4f524db0afa0a7137804391e0a9"},{url:"assets/icon.svg",revision:"15c6bf83aa1dc5f72525950635e0e0d1"},{url:"index.html",revision:"f732ce79fb91f6700af736b4e91e1fee"},{url:"main.4b9e3538a6fa22d3bcf3.js",revision:null},{url:"manifest.webmanifest",revision:"6210caa3e360eded153c695df2909eb3"}],{})}));

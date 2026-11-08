/* Shared top navigation with hover dropdowns. Edit the two lists below to add chapters. */
(function(){
var nav=document.querySelector('nav.gn');if(!nav)return;
var cur=decodeURIComponent((location.pathname.split('/').pop())||'index.html')||'index.html';
var I=[['Chapter 2 · Simple comparisons','chapter 2.html'],['Chapter 3 · Single-factor (ANOVA)','chapter 3.html'],['Chapter 4 · Blocking designs','chapter 4.html'],['Chapter 5 · Factorial designs','chapter 5.html'],['Chapter 6 · 2^k factorial designs','chapter 6.html'],['Upload your own data','upload solver.html']];
var T=[['Chapter 2 · Simple comparisons','textbook chapter 2.html'],['Chapter 3 · Single-factor (ANOVA)','textbook chapter 3.html'],['Chapter 4 · Blocking designs','textbook chapter 4.html'],['Chapter 5 · Factorial designs','textbook chapter 5.html'],['Chapter 6 · 2^k factorial designs','textbook chapter 6.html']];
var u=function(f){return f.replace(/ /g,'%20')};
function grp(label,hub,items){var on=cur===hub||items.some(function(i){return i[1]===cur});
 return '<div class="grp gnd'+(on?' gnon':'')+'"><a class="gnl" href="'+u(hub)+'">'+label+' <span class="gncar">▾</span></a><div class="gnm">'+items.map(function(i){return '<a href="'+u(i[1])+'"'+(i[1]===cur?' class="gncur"':'')+'>'+i[0]+'</a>'}).join('')+'</div></div>'}
nav.innerHTML='<a class="home" href="index.html">DOE · STAT-530</a>'+grp('Interactive solver','interactive solver.html',I)+grp('Textbook solver','textbook solver.html',T);
var st=document.createElement('style');st.id='gn-dd';
st.textContent='.gn .gnd{position:relative}.gn .gnl{color:#8fd3e0;font-weight:600;background:none;margin:0;display:inline-block}.gn .gnl:hover{background:#1b3641;color:#fff}.gn .gnon>.gnl{background:#147d92;color:#fff}.gn .gncar{font-size:11px;opacity:.8}'
+'.gn .gnm{display:none;position:absolute;top:100%;left:0;min-width:240px;background:#0b1a21;border:1px solid #1d3744;border-radius:8px;padding:6px;box-shadow:0 8px 24px rgba(0,0,0,.35);z-index:60}'
+'.gn .gnd:hover>.gnm,.gn .gnd.gnopen>.gnm,.gn .gnd:focus-within>.gnm{display:block}'
+'.gn .gnm a{display:block;white-space:nowrap;padding:8px 12px;border-radius:6px;color:#b9d3dc}.gn .gnm a:hover{background:#1b3641;color:#fff}.gn .gnm a.gncur{background:#147d92;color:#fff}'
+'@media(max-width:600px){.gn .gnm{min-width:210px}}';
document.head.appendChild(st);
var ds=nav.querySelectorAll('.gnd');
ds.forEach(function(d){d.querySelector('.gnl').addEventListener('click',function(e){if(window.matchMedia&&window.matchMedia('(hover: none)').matches){if(!d.classList.contains('gnopen')){e.preventDefault();ds.forEach(function(x){x.classList.remove('gnopen')});d.classList.add('gnopen')}}})});
document.addEventListener('click',function(e){if(!nav.contains(e.target))ds.forEach(function(x){x.classList.remove('gnopen')})});
})();

/* "Download report (PDF)": on every page with a sidebar and a results area, print only the results
   (sidebar and menus hidden, collapsed sections opened, charts fitted to the page) via the browser's Save-as-PDF. */
(function(){
function init(){
 var aside=document.querySelector('aside'),main=document.querySelector('main');if(!aside||!main||document.getElementById('rptbtn'))return;
 var st=document.createElement('style');st.id='rpt-css';
 st.textContent='#rptbtn{display:block;width:100%;margin:12px 0 4px;padding:10px 12px;border:0;border-radius:8px;background:#147d92;color:#fff;font:600 14px/1.3 inherit;cursor:pointer}#rptbtn:hover{background:#0f6475}#rptbtn small{display:block;font-weight:400;font-size:11.5px;opacity:.85;margin-top:2px}'
 +'.rpt-head{display:none}img.rpt-img{display:block;max-width:100%;height:auto;margin:4px auto}'
 +'body.rpt-on .rpt-head{display:block;border-bottom:2px solid #147d92;margin:0 0 14px;padding:0 0 8px}.rpt-head h1{margin:0;font-size:20px;color:#134e63}.rpt-head p{margin:2px 0;font-size:12px;color:#555}'
 +'body.rpt-on nav.gn,body.rpt-on aside{display:none!important}body.rpt-on{display:block!important;background:#fff!important}body.rpt-on main{max-width:none!important;width:720px!important;margin:0 auto!important;padding:0!important}'
 +'@media print{nav.gn,aside,#rptbtn{display:none!important}body{display:block!important;background:#fff!important}main{max-width:none!important;width:auto!important;margin:0!important;padding:0!important}'
 +'.rpt-head{display:block}.card{box-shadow:none!important;break-inside:auto}table,tr,img.rpt-img,h2,h3,h4{break-inside:avoid}img.rpt-img{display:block;width:100%;height:auto;margin:4px auto}.g{grid-template-columns:1fr 1fr!important}.g3{grid-template-columns:1fr 1fr 1fr!important}h2,h3,h4{break-after:avoid}'
 +'button,input[type=file],textarea{display:none!important}*{-webkit-print-color-adjust:exact;print-color-adjust:exact}@page{margin:14mm 12mm}}';
 document.head.appendChild(st);
 var b=document.createElement('button');b.id='rptbtn';b.type='button';b.innerHTML='⬇ Download report (PDF)<small>Opens the print dialog — choose “Save as PDF”</small>';
 var h1=aside.querySelector('h1');if(h1&&h1.nextSibling)aside.insertBefore(b,h1.nextSibling);else aside.insertBefore(b,aside.firstChild);
 var saved=null;
 function context(){var parts=[],pb=document.getElementById('pb'),ch=document.getElementById('ch');
  if(ch&&ch.selectedIndex>=0&&ch.options[ch.selectedIndex])parts.push(ch.options[ch.selectedIndex].text);
  if(pb&&pb.selectedIndex>=0&&pb.options[pb.selectedIndex])parts.push(pb.options[pb.selectedIndex].text);
  try{if(typeof S!=='undefined'&&S&&S.file)parts.push(S.file)}catch(e){}
  return parts.filter(function(x,i,a){return x&&a.indexOf(x)===i})}
 function plots(){return Array.prototype.slice.call(document.querySelectorAll('main .js-plotly-plot'))}
 function resize(){if(window.Plotly&&Plotly.Plots)plots().forEach(function(p){try{Plotly.Plots.resize(p)}catch(e){}})}
 /* replace each live chart with a picture of it, so charts print at full quality and never overlap */
 function snapshot(){if(!(window.Plotly&&Plotly.toImage))return Promise.resolve();
  return Promise.all(plots().map(function(gd){if(gd.offsetWidth<10||gd.style.display==='none')return null;var w=gd.offsetWidth,h=gd.offsetHeight;
   return Plotly.toImage(gd,{format:'png',width:w,height:h,scale:2}).then(function(url){var im=new Image();im.className='rpt-img';im.src=url;im.style.maxWidth=w+'px';gd.parentNode.insertBefore(im,gd.nextSibling);gd.setAttribute('data-rpt-hidden','1');gd.style.display='none'}).catch(function(){})}))}
 function unsnapshot(){document.querySelectorAll('img.rpt-img').forEach(function(im){im.remove()});document.querySelectorAll('[data-rpt-hidden]').forEach(function(gd){gd.style.display='';gd.removeAttribute('data-rpt-hidden')})}
 function prepare(){if(saved)return;
  var ctx=context(),d=new Date();
  saved={title:document.title,details:Array.prototype.map.call(main.querySelectorAll('details'),function(x){return[x,x.open]})};
  main.querySelectorAll('details').forEach(function(x){x.open=true});
  var hd=document.createElement('div');hd.className='rpt-head';hd.id='rpt-head';
  hd.innerHTML='<h1>Design &amp; Analysis of Experiments — report</h1>'+(ctx.length?'<p><b>'+ctx.map(function(x){return x.replace(/[&<>]/g,function(c){return{'&':'&amp;','<':'&lt;','>':'&gt;'}[c]})}).join(' · ')+'</b></p>':'')
   +'<p>Generated '+d.toLocaleString()+' · '+location.href.split('#')[0].replace(/[&<>"]/g,'')+'</p>';
  main.insertBefore(hd,main.firstChild);
  document.title=('DOE report'+(ctx.length?' - '+ctx[ctx.length-1]:'')).replace(/[\\\/:*?"<>|]/g,'-').slice(0,120);
  document.body.classList.add('rpt-on');resize()}
 function restore(){if(!saved)return;var s=saved;saved=null;
  document.body.classList.remove('rpt-on');var hd=document.getElementById('rpt-head');if(hd)hd.remove();
  s.details.forEach(function(x){x[0].open=x[1]});document.title=s.title;unsnapshot();setTimeout(resize,50)}
 b.addEventListener('click',function(){var t=b.innerHTML;b.disabled=true;b.innerHTML='Preparing report…';prepare();
  setTimeout(function(){snapshot().then(function(){b.disabled=false;b.innerHTML=t;window.print();setTimeout(restore,500)})},400)});
 window.addEventListener('beforeprint',prepare);window.addEventListener('afterprint',function(){setTimeout(restore,200)});
}
if(document.readyState==='loading')document.addEventListener('DOMContentLoaded',init);else init();
})();

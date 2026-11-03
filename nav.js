/* Shared top navigation with hover dropdowns. Edit the two lists below to add chapters. */
(function(){
var nav=document.querySelector('nav.gn');if(!nav)return;
var cur=decodeURIComponent((location.pathname.split('/').pop())||'index.html')||'index.html';
var I=[['Chapter 2 · Simple comparisons','chapter 2.html'],['Chapter 3 · Single-factor (ANOVA)','chapter 3.html'],['Chapter 4 · Blocking designs','chapter 4.html'],['Chapter 5 · Factorial designs','chapter 5.html'],['Upload your own data','upload solver.html']];
var T=[['Chapter 2 · Simple comparisons','textbook chapter 2.html'],['Chapter 3 · Single-factor (ANOVA)','textbook chapter 3.html'],['Chapter 4 · Blocking designs','textbook chapter 4.html'],['Chapter 5 · Factorial designs','textbook chapter 5.html']];
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

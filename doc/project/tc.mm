<map version="0.7.1">
<node TEXT="tc">
<node TEXT="parser" POSITION="right">
<node TEXT="interfacing Rats!"/>
</node>
<node TEXT="printer" POSITION="left">
<node TEXT="interfacing Rats!"/>
</node>
<node TEXT="check constraints" POSITION="right">
<edge STYLE="bezier"/>
<node TEXT="recursion with critical functions"/>
<node TEXT="call critical functions by pointer"/>
<node TEXT="no longjmp etc."/>
</node>
<node TEXT="transformer" POSITION="right">
<node TEXT="list of critical functions">
<edge STYLE="bezier"/>
<node TEXT="call graph"/>
</node>
<node TEXT="transformation of each critical function">
<node TEXT="split"/>
<node TEXT="state structs">
<node TEXT="find critical variables"/>
</node>
<node TEXT="abbildung thread id -&gt; state struct index"/>
</node>
<node TEXT="pass the rest"/>
</node>
<node TEXT="configuration" POSITION="left">
<node TEXT="list of blocking calls"/>
<node TEXT="implementation of blocking calls"/>
<node TEXT="exact number of threads"/>
<node TEXT="run method per thread"/>
</node>
<node TEXT="open questions" POSITION="left">
<node TEXT="compiler needs global vision"/>
<node TEXT="multiple critical calls in a function"/>
</node>
<node TEXT="optimizations" POSITION="right">
<node TEXT="state struct per critical call"/>
<node TEXT="avoid thread id -&gt; index mapping if there is no reentrance"/>
</node>
</node>
</map>

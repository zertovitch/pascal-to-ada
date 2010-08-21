-- To avoid the problem of Pascal identifiers same as an Ada keyword

package P2Ada_keywords is

  function Avoid_keyword( id: String ) return String;

end P2Ada_keywords;
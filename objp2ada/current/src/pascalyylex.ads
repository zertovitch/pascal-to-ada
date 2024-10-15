WITH pascal_tokens; USE pascal_tokens;
with yylex;
function pascalyylex return token renames yylex;

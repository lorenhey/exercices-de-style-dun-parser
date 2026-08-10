with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Parser_V7;

procedure Main is
   function S (U : Unbounded_String) return String is (To_String(U));
begin
   Parser_V7.Reset;

   -- incremental real
   Parser_V7.Step("El");
   Parser_V7.Step("filósofo");
   Parser_V7.Step("murió");

   Put_Line("N=" & Natural'Image(Parser_V7.Token_Count));
   Put_Line(S(Parser_V7.Render_Best));

   -- full sentence (si querés, tokenizás afuera y llamás Step por token)
end Main;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Parser_V7 is
   procedure Reset;

   -- Incremental real: agrega 1 token y actualiza spans que terminan en j=N+1
   procedure Step (Token_Surface : String; Token_Score : Long_Float := 0.0);

   -- Devuelve "" si no hay parse. Si hay, árbol bracketed.
   function Render_Best return Unbounded_String;

   -- Útil para debug
   function Token_Count return Natural;

end Parser_V7;

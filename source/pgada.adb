
package body PGAda is
   use type ICS.Chars_Ptr;

   ----------------------
   -- C_String_Or_Null --
   ----------------------

   function C_String_Or_Null (S : in String) return ICS.chars_ptr is
   begin
      if S = "" then
         return ICS.Null_Ptr;
      else
         return ICS.New_String (S);
      end if;
   end C_String_Or_Null;

   ----------
   -- Free --
   ----------

   procedure Free (S : in out ICS.chars_ptr) is
   begin
      if S /= ICS.Null_Ptr then
         ICS.Free (S);
      end if;
   end Free;

end PGAda;

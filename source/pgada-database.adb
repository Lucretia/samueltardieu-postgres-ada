------------------------------------------------------------------------------
--                                                                          --
--                       P G A D A . D A T A B A S E                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--  Copyright (c) Samuel Tardieu 2000                                       --
--  All rights reserved.                                                    --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions      --
--  are met:                                                                --
--  1. Redistributions of source code must retain the above copyright       --
--     notice, this list of conditions and the following disclaimer.        --
--  2. Redistributions in binary form must reproduce the above copyright    --
--     notice, this list of conditions and the following disclaimer in      --
--     the documentation and/or other materials provided with the           --
--     distribution.                                                        --
--  3. Neither the name of Samuel Tardieu nor the names of its contributors --
--     may be used to endorse or promote products derived from this         --
--     software without specific prior written permission.                  --
--                                                                          --
--  THIS SOFTWARE IS PROVIDED BY SAMUEL TARDIEU AND CONTRIBUTORS ``AS       --
--  IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT          --
--  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS       --
--  FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL SAMUEL      --
--  TARDIEU OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,             --
--  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES                --
--  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR      --
--  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)      --
--  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN               --
--  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR            --
--  OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,          --
--  EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                      --
--                                                                          --
------------------------------------------------------------------------------

with System;

with PGAda.Thin;

package body PGAda.Database is
   use type ICS.chars_ptr;
   use type IC.int;

   ----------
   -- Exec --
   ----------

   procedure Exec
     (Connection : in Connections.Connection;
      Query      : in String;
      Result     : out Results.Result;
      Status     : out Results.Exec_Status)
   is
      C_Query : ICS.chars_ptr := ICS.New_String (Query);
   begin
      Result.Actual := PGAda.Thin.PQ_Exec (Connection.Actual, Query => C_Query);

      ICS.Free (C_Query);

      Status := Results.Status (Result);
   end Exec;

   ----------
   -- Exec --
   ----------

   procedure Exec
      (Connection : in Connections.Connection;
       Query      : in String;
       Result     : out Results.Result)
   is
      C_Query : ICS.chars_ptr := ICS.New_String (Query);
   begin
      Result.Actual := PGAda.Thin.PQ_Exec (Connection.Actual, Query => C_Query);

      ICS.Free (C_Query);
   end Exec;

   ----------
   -- Exec --
   ----------

   function Exec
      (Connection : in Connections.Connection;
       Query      : in String) return Results.Result
   is
      Result : Results.Result;
   begin
      Exec (Connection, Query => Query, Result => Result);

      return Result;
   end Exec;

   ----------
   -- Exec --
   ----------

   procedure Exec
     (Connection : in Connections.Connection;
      Query      : in String)
   is
      Result : Results.Result;
   begin
      Exec (Connection, Query => Query, Result => Result);
   end Exec;

   ----------------
   -- Field_Name --
   ----------------

   function Field_Name
     (Result      : in Results.Result;
      Field_Index : in Positive) return String is
   begin
      return ICS.Value (PGAda.Thin.PQ_F_Name (Result.Actual, Field_Index => IC.int (Field_Index) - 1) );
   end Field_Name;

   ----------------
   -- Get_Length --
   ----------------

   function Get_Length
     (Result      : in Results.Result;
      Tuple_Index : in Positive;
      Field_Index : in Positive)
   return Natural is
   begin
      return Natural (PGAda.Thin.PQ_Get_Length (Result.Actual,
                                                Tup_Num   => IC.int (Tuple_Index) - 1,
                                                Field_Num => IC.int (Field_Index) - 1) );
   end Get_Length;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
     (Result      : in Results.Result;
      Tuple_Index : in Positive;
      Field_Index : in Positive)
   return String is
   begin
      return ICS.Value (PGAda.Thin.PQ_Get_Value (Result.Actual,
                                                 Tup_Num   => IC.int (Tuple_Index) - 1,
                                                 Field_Num => IC.int (Field_Index) - 1) );
   end Get_Value;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
     (Result      : in Results.Result;
      Tuple_Index : in Positive;
      Field_Name  : in String)
   return String is
      C_Name : ICS.chars_ptr := ICS.New_String (Field_Name);
      Ret    : constant String :=
         Get_Value (Result,
                    Tuple_Index => Tuple_Index,
                    Field_Index => 1 + Natural (PGAda.Thin.PQ_F_Number (Result.Actual, Field_Index => C_Name) ) );
   begin
      Free (C_Name);

      return Ret;
   end Get_Value;

   -------------
   -- Is_Null --
   -------------

   function Is_Null
     (Result      : in Results.Result;
      Tuple_Index : in Positive;
      Field_Index : in Positive)
   return Boolean is
   begin
      return 1 = PGAda.Thin.PQ_Get_Is_Null (Result.Actual,
                                            Tup_Num   => IC.int (Tuple_Index) - 1,
                                            Field_Num => IC.int (Field_Index) - 1);
   end Is_Null;

   ----------------
   -- OID_Status --
   ----------------

   function OID_Status (Result : in Results.Result) return String is
   begin
      return ICS.Value (PGAda.Thin.PQ_Oid_Status (Result.Actual) );
   end OID_Status;

   function Prepare
     (Connection     : in Connections.Connection;
      Statement_Name : in String;
      Query          : in String;
      Params         : in Natural)
   return Results.Result is
      C_Stmt_Name : ICS.chars_ptr := ICS.New_String (Statement_Name);
      C_Query     : ICS.chars_ptr := ICS.New_String (Query);

      Result : Results.Result;
   begin
      Result.Actual := PGAda.Thin.PQ_Prepare
                         (Connection.Actual,
                          Statement_Name => C_Stmt_Name,
                          Query          => C_Query,
                          Params         => IC.int (Params),
                          Param_Types    => System.Null_Address);

      Free (C_Stmt_Name);
      Free (C_Query);

      return Result;
   end Prepare;

   procedure Prepare
     (Connection     : in Connections.Connection;
      Statement_Name : in String;
      Query          : in String;
      Params         : in Natural)
   is
      Result  : Results.Result;
   begin
      Result := Prepare (Connection,
                         Statement_Name => Statement_Name,
                         Query          => Query,
                         Params         => Params);
   end Prepare;

   function Exec_Prepared
     (Connection     : in Connections.Connection;
      Statement_Name : in String;
      Parameters     : in Parameter_List)
   return Results.Result is
      function To_String (Source : Ada.Strings.Unbounded.Unbounded_String) return String renames Ada.Strings.Unbounded.To_String;

      C_Stmt_Name  : ICS.chars_ptr := ICS.New_String (Statement_Name);
      C_Parameters : aliased array (Parameters'Range) of ICS.chars_ptr;

      Result : Results.Result;
   begin
      for I in Parameters'Range loop
         C_Parameters (I) := ICS.New_String (To_String (Parameters (I) ) );
      end loop;

      Result.Actual := PGAda.Thin.PQ_Exec_Prepared
                         (Connection.Actual,
                          Statement_Name => C_Stmt_Name,
                          Params         => Parameters'Length,
                          Param_Values   => C_Parameters'Address,
                          Param_Lengths  => null,
                          Param_Formats  => null,
                          Result_Format  => 0);

      Free (C_Stmt_Name);

      for I in Parameters'Range loop
         Free (C_Parameters (I) );
      end loop;

      return Result;
   end Exec_Prepared;

   function Exec_Prepared
     (Connection     : in Connections.Connection;
      Statement_Name : in String;
      Parameters     : in Database.Parameter)
   return Results.Result is
   begin
      return Exec_Prepared (Connection,
                            Statement_Name => Statement_Name,
                            Parameters     => Parameter_List'(1 => Parameters) );
   end Exec_Prepared;

end PGAda.Database;

------------------------------------------------------------------------------
--                                                                          --
--                       P G A D A . D A T A B A S E                        --
--                                                                          --
--                                 S p e c                                  --
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

with Ada.Strings.Unbounded;

with PGAda.Connections;
with PGAda.Results;

package PGAda.Database is
   pragma Preelaborate;

   procedure Exec
     (Connection : in Connections.Connection;
      Query      : in String;
      Result     : out Results.Result;
      Status     : out Results.Exec_Status);

   procedure Exec
     (Connection : in Connections.Connection;
      Query      : in String;
      Result     : out Results.Result);

   function Exec
      (Connection : in Connections.Connection;
       Query      : in String)
   return Results.Result;

   procedure Exec
      (Connection : in Connections.Connection;
       Query      : in String);

   function Field_Name
     (Result      : in Results.Result;
      Field_Index : in Positive)
   return String;

   function Get_Value
     (Result      : in Results.Result;
      Tuple_Index : in Positive;
      Field_Index : in Positive)
   return String;

   function Get_Value
     (Result      : in Results.Result;
      Tuple_Index : in Positive;
      Field_Name  : in String)
   return String;

   function Get_Length
     (Result      : in Results.Result;
      Tuple_Index : in Positive;
      Field_Index : in Positive)
   return Natural;

   function Is_Null
     (Result      : in Results.Result;
      Tuple_Index : in Positive;
      Field_Index : in Positive)
   return Boolean;

   function OID_Status (Result : Results.Result) return String;

   subtype Parameter is Ada.Strings.Unbounded.Unbounded_String;

   type Parameter_List is array (Natural range <>) of Parameter;

   function Prepare
     (Connection     : in Connections.Connection;
      Statement_Name : in String;
      Query          : in String;
      Params         : in Natural)
   return Results.Result;

   procedure Prepare
     (Connection     : in Connections.Connection;
      Statement_Name : in String;
      Query          : in String;
      Params         : in Natural);

   function Exec_Prepared
     (Connection     : in Connections.Connection;
      Statement_Name : in String;
      Parameters     : in Parameter_List)
   return Results.Result;

   function Exec_Prepared
     (Connection     : in Connections.Connection;
      Statement_Name : in String;
      Parameters     : in Database.Parameter)
   return Results.Result;

end PGAda.Database;

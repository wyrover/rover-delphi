{   Component(s):
    tcyEditFloat

    Description:
    A Edit for floats

    $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    $  €€€ Accept any PAYPAL DONATION $$$  €
    $      to: mauricio_box@yahoo.com      €
    €€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€

    * ***** BEGIN LICENSE BLOCK *****
    *
    * Version: MPL 1.1
    *
    * The contents of this file are subject to the Mozilla Public License Version
    * 1.1 (the "License"); you may not use this file except in compliance with the
    * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
    *
    * Software distributed under the License is distributed on an "AS IS" basis,
    * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
    * the specific language governing rights and limitations under the License.
    *
    * The Initial Developer of the Original Code is Mauricio
    * (https://sourceforge.net/projects/tcycomponents/).
    *
    * Donations: see Donation section on Description.txt
    *
    * Alternatively, the contents of this file may be used under the terms of
    * either the GNU General Public License Version 2 or later (the "GPL"), or the
    * GNU Lesser General Public License Version 2.1 or later (the "LGPL"), in which
    * case the provisions of the GPL or the LGPL are applicable instead of those
    * above. If you wish to allow use of your version of this file only under the
    * terms of either the GPL or the LGPL, and not to allow others to use your
    * version of this file under the terms of the MPL, indicate your decision by
    * deleting the provisions above and replace them with the notice and other
    * provisions required by the LGPL or the GPL. If you do not delete the
    * provisions above, a recipient may use your version of this file under the
    * terms of any one of the MPL, the GPL or the LGPL.
    *
    * ***** END LICENSE BLOCK *****}
unit cyEditFloat;

interface

uses Classes, Windows, Controls, StdCtrls, SysUtils, cyEdit;

type
  TProcEditFloatNeedDefaultValue = procedure (Sender: TObject; var Value: Double) of object;

  TcyEditFloat = class(TcyAdvBaseEdit)
  private
    FSystemDecimalSeparator: Char;
    FMinValue: Double;
    FAllowNegative: Boolean;
    FMaxValue: Double;
    FOnNeedDefaultValue: TProcEditFloatNeedDefaultValue;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    function GetValue: Double;
    procedure SetValue(const Value: Double);
    procedure SetAllowNegative(const Value: Boolean);
  protected
    procedure KeyPress(var Key: Char); override;
  public
    constructor Create(AOwner: TComponent); override;
    function ValidateText(aText: String): TEditValidateResult; override;
    property Value: Double read GetValue write SetValue;
    property IgnorePressedKey;
    property MaxChars;
  published
    property AllowNegative: Boolean read FAllowNegative write SetAllowNegative default true;
    property MinValue: Double read FMinValue write FMinValue;
    property MaxValue: Double read FMaxValue write FMaxValue;
    property OnNeedDefaultValue: TProcEditFloatNeedDefaultValue read FOnNeedDefaultValue write FOnNeedDefaultValue;
    // Herited :
    // Do not publish ... property CharRules;
    property AllowEmpty;
    property IgnoreRules;
    property ErrorHandling;
    property OnValidateError;
  end;

implementation

{ TcyEditFloat }
constructor TcyEditFloat.Create(AOwner: TComponent);
var
  fs: TFormatSettings;
begin
  inherited;

  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, fs);
  FSystemDecimalSeparator := fs.DecimalSeparator;

  FAllowNegative := true;
  FMinValue := 0;
  FMaxValue := 0;

  // Add rules :
  with CharRules.Add do
  begin
    FromPosition := 1;
    ToPosition := 1;
    AllowedChars := '-0123456789';
  end;

  with CharRules.Add do
  begin
    FromPosition := 2;
    ToPosition := 0;
    AllowedChars := ',0123456789';
  end;

  with CharRules.Add do
  begin
    FromPosition := 0;
    ToPosition := 0;
    ForbiddenCharsSequence := '-' + FSystemDecimalSeparator;
    RepeatOptions.CharsConcerned := FSystemDecimalSeparator;
    RepeatOptions.MaxRepeat := 1;
  end;
end;

procedure TcyEditFloat.SetAllowNegative(const Value: Boolean);
begin
  FAllowNegative := Value;

  if AllowNegative
  then CharRules[0].AllowedChars := '-0123456789'
  else CharRules[0].AllowedChars := '0123456789';
end;

procedure TcyEditFloat.SetValue(const Value: Double);
begin
  Text := FloatToStr(Value);
end;

function TcyEditFloat.GetValue: Double;
var Rslt: Extended;
begin
  if Text = '' then
    Result := 0
  else
    if TryStrToFloat(Text, Rslt)
    then Result := Rslt
    else Result := 0;
end;

function TcyEditFloat.ValidateText(aText: String): TEditValidateResult;
var Rslt: Extended;
begin
  Result := evValid;

  if aText = '' then
    Result := evInvalidValue
  else
    if not TryStrToFloat(aText, Rslt) then
      Result := evInvalidValue
    else
      if FMinValue <> FMaxValue then
        if Rslt > FMaxValue then
          Result := evOutOfMaxRange
        else
          if Rslt < FMinValue then
            Result := evOutOfMinRange;
end;

procedure TcyEditFloat.KeyPress(var Key: Char);
begin
  // Replace char by decimal separator :
  if Key <> FSystemDecimalSeparator then
    if Key in ['.', ','] then
      Key := FSystemDecimalSeparator;

  inherited;
end;

procedure TcyEditFloat.CMExit(var Message: TCMExit);
var
  DefaultValue: Double;
begin
  if (not AllowEmpty) and (Text = '') then
    if Assigned(FOnNeedDefaultValue) then
    begin
      DefaultValue := 0;
      FOnNeedDefaultValue(Self, DefaultValue);
      Value := DefaultValue;
    end;

  Inherited;
end;
end.

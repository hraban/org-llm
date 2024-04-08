# Copyright © 2024  Hraban Luyat
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, version 3 of the License.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

{
  description = "Emacs package for LLM conversations in a .org document";

  outputs = { self }: {
    emacsPackages.default = {
      lib
    , trivialBuild
    , dash
    , s
    , llm
    }: trivialBuild rec {
      pname = "org-llm";
      version = "20240408";
      src = lib.cleanSource ./.;
      propagatedUserEnvPkgs = [ dash llm s ];
      buildInputs = propagatedUserEnvPkgs;
      meta.license = lib.licenses.agpl3Only;
    };
  };
}

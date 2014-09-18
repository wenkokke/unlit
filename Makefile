src/Unlit/String.hs: src/Unlit/Text.lhs
	cat src/Unlit/Text.lhs | unlit | sed -e 's/Text/String/' | sed -e 's/Data.String.Lazy/Data.List/' | sed -e 's/Data.List.IO/Prelude/' > src/Unlit/String.hs

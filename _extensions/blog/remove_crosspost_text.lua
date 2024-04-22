function Plain (str)
  el = str.content
  text = pandoc.utils.stringify(el)
  if text == 'Reproducibility receipt' or text=='(Joint with Nathan Lauster and cross-posted at HomeFreeSociology)' then
    return pandoc.Str('')
  end
end
function Meta (m)
  print(m)
  if m.abstract then
    m.removekey('abstract')
    return m
  end
end

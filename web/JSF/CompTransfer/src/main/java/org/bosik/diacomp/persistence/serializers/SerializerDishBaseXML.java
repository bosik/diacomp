package org.bosik.diacomp.persistence.serializers;

@Deprecated
public class SerializerDishBaseXML
{
	// private static final String TAG = SerializerDishBaseXML.class.getSimpleName();
	//
	// @Override
	// public MemoryBase<DishItem> read(String xmlData)
	// {
	// if ("".equals(xmlData))
	// {
	// return new MemoryBase<DishItem>();
	// }
	//
	// try
	// {
	// /**/long time = System.currentTimeMillis();
	// Document doc = XmlUtils.readDocument(xmlData);
	// Element root = doc.getDocumentElement();
	// NodeList nodes = root.getChildNodes();
	//
	// MemoryBase<DishItem> dishBase = new MemoryBase<DishItem>();
	// dishBase.beginUpdate();
	// dishBase.clear();
	//
	// for (int i = 0; i < nodes.getLength(); i++)
	// {
	// Node node = nodes.item(i);
	//
	// if (node instanceof Element)
	// {
	// Element x = (Element) node;
	//
	// DishItem food = new DishItem(x.getAttribute("name"));
	// // food.setRelProts(Double.parseDouble(x.getAttribute("prots")));
	// // food.setRelFats(Double.parseDouble(x.getAttribute("fats")));
	// // food.setRelCarbs(Double.parseDouble(x.getAttribute("carbs")));
	// // food.setRelValue(Double.parseDouble(x.getAttribute("val")));
	// // food.setFromTable(x.getAttribute("table").equalsIgnoreCase("true"));
	//
	// dishBase.add(food);
	// }
	// }
	//
	// dishBase.setVersion(Integer.parseInt(root.getAttribute("version")));
	// dishBase.endUpdate();
	//
	// /**/Log.v(
	// TAG,
	// String.format("DishBase deserialized in %d msec, total items: %d", System.currentTimeMillis()
	// - time, dishBase.count()));
	// return dishBase;
	// }
	// catch (Exception e)
	// {
	// throw new RuntimeException(e);
	// }
	// }
	//
	// @Override
	// public String write(MemoryBase<DishItem> foodBase)
	// {
	// Document doc = XmlUtils.newDocument();
	//
	// Element root = doc.createElement("foods");
	// root.setAttribute("version", String.valueOf(foodBase.getVersion()));
	// doc.appendChild(root);
	//
	// for (int i = 0; i < foodBase.count(); i++)
	// {
	// DishItem food = foodBase.get(i);
	//
	// Element childelement = doc.createElement("food");
	// childelement.setAttribute("name", String.valueOf(food.getName()));
	// // childelement.setAttribute("prots", String.valueOf(food.getRelProts()));
	// // childelement.setAttribute("fats", String.valueOf(food.getRelFats()));
	// // childelement.setAttribute("carbs", String.valueOf(food.getRelCarbs()));
	// // childelement.setAttribute("val", String.valueOf(food.getRelValue()));
	// // childelement.setAttribute("table", String.valueOf(food.getFromTable()));
	// root.appendChild(childelement);
	// }
	//
	// try
	// {
	// return XmlUtils.writeDocument(doc);
	// }
	// catch (TransformerException e)
	// {
	// throw new RuntimeException(e);
	// }
	// }
	//
	// @Override
	// public List<MemoryBase<DishItem>> readAll(String data)
	// {
	// throw new UnsupportedOperationException("Not implemented");
	// }
	//
	// @Override
	// public String writeAll(List<MemoryBase<DishItem>> object)
	// {
	// throw new UnsupportedOperationException("Not implemented");
	// }
}

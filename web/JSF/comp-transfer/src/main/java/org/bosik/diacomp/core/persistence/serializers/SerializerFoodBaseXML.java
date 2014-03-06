package org.bosik.diacomp.core.persistence.serializers;

/**
 * Provides methods for foodbase (de)serialization in XML format
 * 
 * @author Bosik
 * 
 */
@Deprecated
public class SerializerFoodBaseXML
{
	// private static final String TAG = SerializerFoodBaseXML.class.getSimpleName();

	// @Override
	// public MemoryBase<FoodItem> read(String xmlData)
	// {
	// throw new UnsupportedOperationException("Not implemented");
	// if ("".equals(xmlData))
	// {
	// return new MemoryBase<FoodItem>();
	// }
	//
	// try
	// {
	// /**/long time = System.currentTimeMillis();
	//
	// Document doc = XmlUtils.readDocument(xmlData);
	// Element root = doc.getDocumentElement();
	// NodeList nodes = root.getChildNodes();
	//
	// MemoryBase<FoodItem> foodBase = new MemoryBase<FoodItem>();
	// foodBase.beginUpdate();
	// foodBase.clear();
	//
	// for (int i = 0; i < nodes.getLength(); i++)
	// {
	// Node node = nodes.item(i);
	//
	// if (node instanceof Element)
	// {
	// Element x = (Element) node;
	//
	// FoodItem food = new FoodItem(x.getAttribute("name"));
	// food.setId(x.getAttribute("id"));
	// food.setRelProts(Double.parseDouble(x.getAttribute("prots")));
	// food.setRelFats(Double.parseDouble(x.getAttribute("fats")));
	// food.setRelCarbs(Double.parseDouble(x.getAttribute("carbs")));
	// food.setRelValue(Double.parseDouble(x.getAttribute("val")));
	// food.setFromTable(x.getAttribute("table").equalsIgnoreCase("true"));
	//
	// if (x.hasAttribute("tag"))
	// {
	// food.setTag(Integer.parseInt(x.getAttribute("tag")));
	// }
	// else
	// {
	// food.setTag(0);
	// }
	//
	// foodBase.add(food);
	// }
	// }
	//
	// foodBase.setVersion(Integer.parseInt(root.getAttribute("version")));
	// foodBase.endUpdate();
	//
	// /**/Log.v(
	// TAG,
	// String.format("FoodBase deserialized in %d msec, total items: %d",
	// System.currentTimeMillis()
	// - time, foodBase.count()));
	//
	// return foodBase;
	// }
	// catch (Exception e)
	// {
	// throw new RuntimeException(e);
	// }
	// }

	// @Override
	// public String write(MemoryBase<FoodItem> foodBase)
	// {
	// throw new UnsupportedOperationException("Not implemented");
	// /**/long time = System.currentTimeMillis();
	//
	// Document doc = XmlUtils.newDocument();
	//
	// Element root = doc.createElement("foods");
	// root.setAttribute("version", String.valueOf(foodBase.getVersion()));
	// doc.appendChild(root);
	//
	// for (int i = 0; i < foodBase.count(); i++)
	// {
	// FoodItem food = foodBase.get(i);
	//
	// Element childelement = doc.createElement("food");
	// childelement.setAttribute("id", String.valueOf(food.getId()));
	// childelement.setAttribute("name", String.valueOf(food.getName()));
	// childelement.setAttribute("prots", String.valueOf(food.getRelProts()));
	// childelement.setAttribute("fats", String.valueOf(food.getRelFats()));
	// childelement.setAttribute("carbs", String.valueOf(food.getRelCarbs()));
	// childelement.setAttribute("val", String.valueOf(food.getRelValue()));
	// childelement.setAttribute("table", String.valueOf(food.getFromTable()));
	// childelement.setAttribute("tag", String.valueOf(food.getTag()));
	// root.appendChild(childelement);
	// }
	//
	// /**/Log.v(TAG, String.format("FoodBase serialized in %d msec, total items: %d",
	// System.currentTimeMillis()
	// - time, foodBase.count()));
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
	// public List<MemoryBase<FoodItem>> readAll(String data)
	// {
	// throw new UnsupportedOperationException("Not implemented");
	// }
	//
	// @Override
	// public String writeAll(List<MemoryBase<FoodItem>> object)
	// {
	// throw new UnsupportedOperationException("Not implemented");
	// }
}

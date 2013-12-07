package org.bosik.compensation.utils;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import org.w3c.dom.Document;
import org.xml.sax.SAXException;

/**
 * Предоставляет статические методы для работы с xml:
 * <ul>
 * <li>{@link #newDocument()}</li>
 * <li>{@link #readDocument(String)}</li>
 * <li>{@link #writeDocument(Document)}</li>
 * </ul>
 * 
 * @author Bosik
 * 
 */
public class XmlUtils
{
	private static final String		TAG	= XmlUtils.class.getSimpleName();
	private static DocumentBuilder	builder;
	private static Transformer		transformer;

	static
	{
		try
		{
			builder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
		}
		catch (ParserConfigurationException e)
		{
			throw new RuntimeException("Can not initialize " + TAG + "'s builder", e);
		}

		try
		{
			transformer = TransformerFactory.newInstance().newTransformer();
		}
		catch (TransformerConfigurationException e)
		{
			throw new RuntimeException("Can not initialize " + TAG + "'s transformer", e);
		}
	}

	public static Document newDocument()
	{
		return builder.newDocument();
	}

	public static Document readDocument(String xmlData) throws SAXException, IOException
	{
		InputStream stream = new ByteArrayInputStream(xmlData.getBytes("UTF-8"));
		return builder.parse(stream);
	}

	public static String writeDocument(Document doc) throws TransformerException
	{
		DOMSource source = new DOMSource(doc);
		OutputStream stream = new ByteArrayOutputStream();
		StreamResult result = new StreamResult(stream);
		transformer.transform(source, result);
		return stream.toString();
	}
}
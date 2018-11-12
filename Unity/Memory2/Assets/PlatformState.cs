using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System.Security.Cryptography;
using System;

public class PlatformState : MonoBehaviour {

	// Use this for initialization
	public List<CardState> rotatedCards = new List<CardState>();
	public List<Material> materialOptions;
	AudioSource[] platformAudio;
	TextMesh winText;
	[SerializeField]public List<GameObject> cards;

	public int score;
	void Start () {
		score = 0;
		platformAudio = GetComponents<AudioSource>();
		cards.Clear();
		foreach (Transform child in transform)
		{
			if (child.tag == "Card")
			{
				cards.Add(child.gameObject);
			}
		}
		setMaterial();
		winText = GetComponentInChildren<TextMesh>();
		winText.text = "";
	}
	
	public void rotateTwo()
	{
		CardState temp1 = rotatedCards[0];
		CardState temp2 = rotatedCards[1];

		if (temp1.getMaterialName() == temp2.getMaterialName())
		{
			temp1.disableCard();
			temp2.disableCard();
		}
		else
		{
			temp1.rotateBack = true;
			temp2.rotateBack = true;

		}
		rotatedCards.Clear();
	}

	public int rotatedCount()
	{
		return rotatedCards.Count;
	}
	
	// Update is called once per frame
	void Update () {
		if(score == 8)
		{
			playApplause();
			winText.text = "YOU WON!";
			score = -1;
		}
	}

	void setMaterial()
	{
		CardState cardscript;
		cards.Shuffle();
		for (int i = 0; i < cards.Count; i++)
		{
			cardscript = cards[i].GetComponent<CardState>();
			cardscript.setMaterial(materialOptions[i%8]);
		}
	}
	public void playDing()
	{
		platformAudio[1].Play();
	}
	public void playApplause()
	{
		platformAudio[0].Play();
	}
}
static class MyExtensions
{
	public static void Shuffle<T>(this IList<T> list)
	{
		RNGCryptoServiceProvider provider = new RNGCryptoServiceProvider();
		int n = list.Count;
		while (n > 1)
		{
			byte[] box = new byte[1];
			do provider.GetBytes(box);
			while (!(box[0] < n * (Byte.MaxValue / n)));
			int k = (box[0] % n);
			n--;
			T value = list[k];
			list[k] = list[n];
			list[n] = value;
		}
	}


}

using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class CardState : MonoBehaviour {

	// Use this for initialization
	[SerializeField] float speed = 700f;
	public bool rotateFront;
	public bool rotateBack;
	bool rotated;
	PlatformState platform;
	AudioSource audio;

	void Start () {
		rotateFront = false;
		rotateBack = false;
		platform = GetComponentInParent<PlatformState>();
		audio = GetComponent<AudioSource>();
	}
	
	// Update is called once per frame
	void Update () {
		if (rotateFront)
		{
			transform.Rotate(Vector3.forward, speed * Time.deltaTime);
			if (transform.eulerAngles.z > 180f)
			{
				transform.eulerAngles = new Vector3(0, 0, 180f);
				rotateFront = false;
				rotated = true;
			}	
		}
		if (rotateBack)
		{
			backRotation();
		}


	}

	void OnMouseDown ()
	{	
		if (platform.rotatedCount() == 2)
		{
			platform.rotateTwo();
		}
		if (!rotated)
		{
			if (platform.rotatedCount() == 0)
			{
				platform.rotatedCards.Add(this);
				rotateFront = true;
				audio.Play();
				
			}
			else if (platform.rotatedCount() == 1)
			{
				if (platform.rotatedCards[0] != this)
				{
					platform.rotatedCards.Add(this);
					rotateFront = true;
					audio.Play();
					if (platform.rotatedCards[0].getMaterialName() == this.getMaterialName())
					{
						platform.playDing();
						platform.score++;
					}
				}
			}
		}
	}

	public void backRotation()
	{
		transform.Rotate(Vector3.back, speed * Time.deltaTime);
			
		if (transform.rotation.z <= 0f)
		{
			transform.eulerAngles = new Vector3(0, 0, 0f);
			rotateBack = false;
			rotated = false;
		}	
	}

	public void setMaterial(Material materialOption)
	{
		foreach (Transform child in transform)
		{
			if (child.gameObject.tag == "BackSide")
			{
				child.GetComponent<Renderer>().material = materialOption;

			}
		}
 	}
	public string getMaterialName()
	{
		string temp = null;
		foreach (Transform child in transform)
		{
			if (child.gameObject.tag == "BackSide")
			{
				temp = child.GetComponent<Renderer>().material.name;
				break;
			}
		}
		return temp;
	}

	public void disableCard()
	{
		this.GetComponent<BoxCollider>().enabled = false;
	}
}
	

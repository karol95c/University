using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Rocket : MonoBehaviour {

	// Use this for initialization
	[SerializeField] float rcsThrust = 250f;
	[SerializeField] float mainThrust = 50f;
	Rigidbody rigidBody;
	AudioSource audio;
	
	void Start () {
		rigidBody = GetComponent<Rigidbody>();
		audio = GetComponent<AudioSource>();
	}
	
	// Update is called once per frame
	void Update () {
		Thrust();
		Rotate();
		
	}

	private void Rotate()
	{
		rigidBody.freezeRotation = true;

		float rotationThisFrame = Time.deltaTime * rcsThrust;
		if (Input.GetKey(KeyCode.A))
		{
			transform.Rotate(Vector3.forward * rotationThisFrame);
		}
		else if (Input.GetKey(KeyCode.D))
		{
			transform.Rotate(-Vector3.forward * rotationThisFrame);
		}

		rigidBody.freezeRotation = false;
	}
	private void Thrust()
	{ 
		if (Input.GetKey(KeyCode.Space))
		{
			rigidBody.AddRelativeForce(Vector3.up * mainThrust);
			if (!audio.isPlaying)audio.Play();
		}
		if (Input.GetKeyUp(KeyCode.Space))
		{
			audio.Stop();
		}

	}
	
	void OnCollisionEnter(Collision collision)
	{
		switch (collision.gameObject.tag)
		{
			case "Friendly" :
				//do nothing;
				break;
		}
	}
}
